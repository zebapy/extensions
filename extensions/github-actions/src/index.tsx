import { Icon, Image, MenuBarExtra, open, openCommandPreferences } from "@raycast/api";
import { Fragment } from "react";
import { baseRepoUrl, useWorkflows, Workflow } from "./hooks";
import { getActionIcon, getTimeAgo, groupByWorkflowName, truncate } from "./utils";

const { Item, Submenu, Separator } = MenuBarExtra;

/**
 * Render the list of jobs for a given workflow
 */
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

/**
 * Render a list of workflows, grouped by name.
 */
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

/**
 * Render the root command and menu bar extra
 */
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
