import { getPreferenceValues, LocalStorage, openCommandPreferences, showToast, Toast } from "@raycast/api";
import { Octokit } from "octokit";
import { useEffect, useState } from "react";

const { githubPAT, githubRepo } = getPreferenceValues<{
  githubPAT: string;
  githubRepo: string;
}>();
const [owner, repo] = githubRepo.split("/");

export const baseRepoUrl = `https://github.com/${owner}/${repo}`;

const octokit = initOctokit({ auth: githubPAT });

async function fetchJobsForRun(runId: number) {
  try {
    const resp = await octokit.rest.actions.listJobsForWorkflowRun({
      owner,
      repo,
      run_id: runId,
    });

    return resp.data.jobs;
  } catch (err) {
    console.error(`Error fetching jobs for run ${runId}`, err);

    return [];
  }
}

async function fetchRepoWorkflows() {
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
          jobs: await fetchJobsForRun(run.id),
        };
      })
    );
  } catch (err) {
    console.error("Error fetching workflows", err);

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

export type Workflow = Awaited<ReturnType<typeof fetchRepoWorkflows>>[0];

export function useWorkflows() {
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
}
