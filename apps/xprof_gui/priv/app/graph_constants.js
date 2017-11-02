export const COLUMNS = {
  time: "time",
  count: "count",
  max: "max",
  p99: "p99",
  p90: "p90",
  p75: "p75",
  p50: "p50",
  p25: "p25",
  mean: "mean",
  min: "min",
  median: "median",
  memsize: "memsize",
  stddev: "stddev",
};
export const COLUMNS_TO_NAMES = {
  time: "time",
  count: "count",
  max: "max",
  p99: "99th perc",
  p90: "90th perc",
  p75: "75th perc",
  p50: "50th perc",
  p25: "25th perc",
  mean: "mean",
  min: "min",
  median: "median",
  memsize: "memsize",
  stddev: "stddev",
};
export const NAMES_TO_COLUMNS = {
  time: "time",
  count: "count",
  max: "max",
  "99th perc": "p99",
  "90th perc": "p90",
  "75th perc": "p75",
  "50th perc": "p50",
  "25th perc": "p25",
  mean: "mean",
  min: "min",
  median: "median",
  memsize: "memsize",
  stddev: "stddev",
};
export const POINT = { show: false };
export const GRID = {
  x: { show: true },
  y: { show: true }
};
export const AXIS = {
  x: {
    type: "timeseries",
    tick: {
      // Divide the span of 5*60 seconds nicely
      count: 12,
      fit: false,
      outer: false,
      format: "%H:%M:%S"
    }
  },
  y: {
    min: 0,
    // Would be nice to have some paddig but it should not screw up tick positions
    // Padding: { bottom: 5 }, // in pixels
    padding: { bottom: 2 },
    label: { text: "Call time", position: "outer-middle" },
    tick: {
      // Count: 6, // would be nice to have a bit less ticks then the default but by using count:
      // "The position of the ticks will be calculated precisely, so the values on the ticks will not be rounded nicely."
      outer: false,
      format: function(d) {
        return d3.format(".2s")(d / 1000000) + "s";
      }
    }
  },
  y2: {
    show: true,
    min: 0,
    padding: { bottom: 2 },
    show: true,
    label: { text: "Call count", position: "outer-middle" },
    tick: {
      outer: false
    }
  }
};
export const TRANSITION = { duration: 0 };
export const DATA = {
  x: COLUMNS.time,
  hide: [
    COLUMNS_TO_NAMES.max,
    COLUMNS_TO_NAMES.p90,
    COLUMNS_TO_NAMES.p75,
    COLUMNS_TO_NAMES.p50,
  ],
  axes: {
    count: "y2"
  },
  colors: {
    "count": "#98FB98",
    "max": "#8C2A04",
    "99th perc": "#E24806",
    "90th perc": "#E24806",
    "75th perc": "#E26606",
    "50th perc": "#E26606",
    "mean": "#FFAA00",
    "min": "#D3D004",
  }
};
export const MAX_DPS = 300;
export const GET_SAMPLES_INTERVAL = 1000;
export const GET_FUNS_INTERVAL = 500;
export const CAPTURE_CALLS_INTERVAL = 500;
export const GET_STATUS_INTERVAL = 1000;