import {
  getPreferenceValues,
  Icon,
  Image,
  LocalStorage,
  MenuBarExtra,
  open,
  openCommandPreferences,
  showToast,
  Toast,
} from "@raycast/api";
import { Fragment, useEffect, useState } from "react";
import { Octokit } from "octokit";
import { getTimeAgo } from "./getTimeAgo";

const { Item, Submenu, Separator } = MenuBarExtra;

const { githubPAT, githubRepo } = getPreferenceValues();
const [owner, repo] = githubRepo.split("/");

const baseRepoUrl = `https://github.com/${owner}/${repo}`;

const octokit = initOctokit({ auth: githubPAT });

type Workflow = Awaited<ReturnType<typeof fetchRepoWorkflows>>[0];

const useWorkflows = () => {
  const [isLoading, setIsLoading] = useState(true);
  const [workflows, setWorkflows] = useState<Workflow[]>([]);

  async function loadWorkflows() {
    try {
      if (!validatePreferences()) {
        return;
      }

      const workflows = await fetchRepoWorkflows();

      await persistState(workflows);

      let cachedWorkflows = await fetchLocalState();

      setWorkflows(cachedWorkflows);

      setIsLoading(false);
    } catch (error) {
      console.error("Failed loading workflows", error);
      setIsLoading(false);
      throw error;
    }
  }

  useEffect(() => {
    loadWorkflows();
  }, []);

  return {
    workflows,
    isLoading,
  };
};

// https://developers.raycast.com/api-reference/user-interface/icons-and-images
function getActionIcon({ status, conclusion }: Pick<Workflow, "status" | "conclusion">) {
  if (status === "in_progress") {
    return Icon.CircleProgress75;
  }

  if (conclusion === "success") {
    return Icon.Check;
  }

  if (conclusion === "failure") {
    return Icon.XMarkCircle;
  }

  return Icon.MinusCircle;
}

function groupByWorkflowName(workflows: Workflow[]) {
  const groups = workflows.reduce<Record<string, Workflow[]>>((acc, workflow) => {
    const { name } = workflow;

    if (!name) return acc;

    if (!acc[name]) {
      acc[name] = [];
    }

    acc[name].push(workflow);

    return acc;
  }, {});

  return Object.entries(groups).map(([name, workflows]) => ({ name, workflows }));
}

function WorkflowJobs({ jobs }: { jobs: Workflow["jobs"] }) {
  return (
    <>
      {jobs.map((job) => {
        return (
          <Submenu key={job.id} title={job.name} icon={getActionIcon(job)}>
            <Item
              icon={Icon.ArrowRight}
              title="Open Job in browser"
              onAction={() => {
                if (job.html_url) open(job.html_url);
              }}
            />

            {job.steps ? (
              <>
                <Separator />
                <Item icon={Icon.BulletPoints} title="Steps" />
                {job.steps.map((step) => {
                  return (
                    <Item
                      key={step.number}
                      icon={getActionIcon(step)}
                      title={step.name}
                      onAction={() => {
                        open(`${job.html_url}#step:${step.number}:1`);
                      }}
                    />
                  );
                })}
              </>
            ) : null}
          </Submenu>
        );
      })}
    </>
  );
}

function truncate(text: string) {
  const len = 40;
  if (text.length > len) {
    return text.slice(0, len) + "...";
  }
  return text;
}

function Workflows({ workflows }: { workflows: Workflow[] }) {
  const groupedWorkflows = groupByWorkflowName(workflows);

  return (
    <>
      {groupedWorkflows.map(({ name, workflows }) => {
        return (
          <Fragment key={name}>
            <Item title={name} />

            {workflows.map((workflow) => {
              const title = `${workflow.head_branch} - ${getTimeAgo(new Date(workflow.updated_at))}`;

              return (
                <Submenu key={workflow.id} icon={getActionIcon(workflow)} title={title}>
                  <Item icon={Icon.ArrowRight} title="Open Run in browser" onAction={() => open(workflow.html_url)} />

                  {workflow.pull_requests?.map((pr) => {
                    return (
                      <Item
                        key={pr.id}
                        icon={Icon.Hashtag}
                        title={`View pull request #${pr.number}`}
                        onAction={() => open(`${baseRepoUrl}/pull/${pr.number}`)}
                      />
                    );
                  })}

                  <Separator />

                  {workflow.actor ? (
                    <Item
                      icon={{
                        source: workflow.actor.avatar_url,
                        mask: Image.Mask.Circle,
                      }}
                      title={workflow.actor.login}
                      onAction={() => {
                        if (workflow.actor?.html_url) {
                          open(workflow.actor.html_url);
                        }
                      }}
                    />
                  ) : null}

                  {workflow.head_commit ? (
                    <Item
                      icon={Icon.Message}
                      title={truncate(workflow.head_commit.message)}
                      onAction={() => open(`${baseRepoUrl}/commit/${workflow.head_sha}`)}
                    />
                  ) : null}

                  <Separator />

                  <Item icon={Icon.BulletPoints} title="Jobs" />

                  <WorkflowJobs jobs={workflow.jobs} />
                </Submenu>
              );
            })}
          </Fragment>
        );
      })}
    </>
  );
}

export default function Command() {
  const { isLoading, workflows } = useWorkflows();

  return (
    <MenuBarExtra
      icon={workflows[0] ? getActionIcon(workflows[0]) : Icon.List}
      isLoading={isLoading}
      title="GitHub Actions"
    >
      <Item icon={Icon.ArrowRight} title="Open Workflows in browser" onAction={() => open(`${baseRepoUrl}/actions`)} />

      <Separator />

      <Workflows workflows={workflows} />

      <Separator />

      <Item icon={Icon.Gear} title="Preferences" onAction={() => openCommandPreferences()} />
    </MenuBarExtra>
  );
}

async function getJobsForRun(run_id: number) {
  try {
    const resp = await octokit.rest.actions.listJobsForWorkflowRun({
      owner,
      repo,
      run_id,
    });

    return resp.data.jobs;
  } catch (err) {
    console.error(`error fetching jobs for run ${run_id}`, err);

    return [];
  }
}

async function fetchRepoWorkflows() {
  console.log("fetching workflows", {
    repo,
    owner,
  });

  try {
    const resp = await octokit.rest.actions.listWorkflowRunsForRepo({
      owner,
      repo,
      per_page: 10,
    });

    return await Promise.all(
      resp.data.workflow_runs.map(async (run) => {
        return {
          ...run,
          jobs: await getJobsForRun(run.id),
        };
      })
    );
  } catch (err) {
    console.error("error fetching workflows", err);

    return [];
  }
}

const STORAGE_KEY = "workflows";
async function persistState(workflows: Workflow[]) {
  LocalStorage.setItem(STORAGE_KEY, JSON.stringify(workflows));
}

async function fetchLocalState() {
  let state: Workflow[];

  const stateString = await LocalStorage.getItem<string>(STORAGE_KEY);
  if (stateString) {
    state = JSON.parse(stateString) as Workflow[];
  } else {
    state = [];
  }

  return state;
}

function initOctokit(options: { auth: string }): Octokit {
  return new Octokit({
    auth: options.auth,
    throttle: {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      onRateLimit: (retryAfter: number, options: any) => {
        console.warn(`Request quota exhausted for request ${options.method} ${options.url}`);
        if (options.request.retryCount === 0) {
          console.info(`Retrying after ${retryAfter} seconds!`);
          return true;
        }
      },
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      onSecondaryRateLimit: (retryAfter: number, options: any) => {
        console.warn(`SecondaryRateLimit detected for request ${options.method} ${options.url}`);
      },
    },
  });
}

function validatePreferences(): boolean {
  if (githubRepo.trim().includes("/")) {
    return true;
  }
  showToast({
    title: "Invalid repository - enter owner/repo format in preferences",
    style: Toast.Style.Failure,
    primaryAction: {
      title: "Open Preferences",
      onAction: async () => {
        openCommandPreferences();
      },
    },
  });
  return false;
}
