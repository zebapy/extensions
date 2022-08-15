// sourced from: https://gist.github.com/pomber/6195066a9258d1fb93bb59c206345b38
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
