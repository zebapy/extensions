import { Icon } from "@raycast/api";
import { Workflow } from "./hooks";

/**
 * Turn an end date, with an optional start date (defaulting to now),
 * into a human readable duration.
 * sourced from: https://gist.github.com/pomber/6195066a9258d1fb93bb59c206345b38
 */
export function getTimeAgo(endTime: Date, startTime = new Date()) {
  const MINUTE = 60;
  const HOUR = MINUTE * 60;
  const DAY = HOUR * 24;
  const WEEK = DAY * 7;
  const MONTH = DAY * 30;
  const YEAR = DAY * 365;

  const diff = startTime.getTime() - endTime.getTime();
  const secondsAgo = Math.round(diff / 1000);
  let divisor = null;
  let unit = null;

  if (secondsAgo < MINUTE) {
    return secondsAgo + "s";
  } else if (secondsAgo < HOUR) {
    [divisor, unit] = [MINUTE, "m"];
  } else if (secondsAgo < DAY) {
    [divisor, unit] = [HOUR, "hr"];
  } else if (secondsAgo < WEEK) {
    [divisor, unit] = [DAY, "d"];
  } else if (secondsAgo < MONTH) {
    [divisor, unit] = [WEEK, "wk"];
  } else if (secondsAgo < YEAR) {
    [divisor, unit] = [MONTH, "mo"];
  } else if (secondsAgo > YEAR) {
    [divisor, unit] = [YEAR, "yr"];
  }

  if (divisor === null) throw new Error("no divisor");

  const count = Math.floor(secondsAgo / divisor);
  return `${count}${unit}`;
}

/**
 * Truncate text to 40 chars and add ellipsis after
 */
export function truncate(text: string) {
  const len = 40;
  if (text.length > len) {
    return text.slice(0, len) + "...";
  }
  return text;
}

export function groupByWorkflowName(workflows: Workflow[]) {
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

// https://developers.raycast.com/api-reference/user-interface/icons-and-images
export function getActionIcon({ status, conclusion }: Pick<Workflow, "status" | "conclusion">) {
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
