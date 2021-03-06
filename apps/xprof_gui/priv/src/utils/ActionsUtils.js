import { isEmpty, sortBy, last, takeRight, initial, range } from 'lodash';
import { callsDecision, dpsDecision, sortItems } from '../utils';
import { getLastCallsForFunction } from '../selectors';
import {
  CAPTURE_CALLS_ACTION,
  DPS_LIMIT,
  DPS_ACTION,
  CALLS_COLUMNS,
  SORT,
  NOTIFICATIONS,
  COLUMNS,
} from '../constants';
import {
  setCallsControl,
  addNotification,
  setLastAsCurrentPage,
  addY,
} from '../actions';
import * as XProf from '../api';

export const determineNextCallsForFun = (json, lastCalls, calls, name) => {
  let callsForFun;

  switch (callsDecision(json, lastCalls)) {
    case CAPTURE_CALLS_ACTION.APP_INITIALIZATION:
    case CAPTURE_CALLS_ACTION.START_FIRST_CALLS_CAPTURE:
      callsForFun = [
        {
          capture_id: json.capture_id,
          items: json.items,
          has_more: json.has_more,
          sort: {
            items: json.items.map(item => ({ ...item, expanded: false })),
            column: CALLS_COLUMNS.ID,
            order: SORT.ASCENDING,
          },
        },
      ];
      break;
    case CAPTURE_CALLS_ACTION.START_NEXT_CALLS_CAPTURE:
      callsForFun = [
        ...calls[name],
        {
          capture_id: json.capture_id,
          items: json.items,
          has_more: json.has_more,
          sort: {
            items: json.items.map(item => ({ ...item, expanded: false })),
            column: CALLS_COLUMNS.ID,
            order: SORT.ASCENDING,
          },
        },
      ];
      break;
    case CAPTURE_CALLS_ACTION.CAPTURING:
    case CAPTURE_CALLS_ACTION.LAST_CALLS_CAPTURE:
      callsForFun = [
        ...initial(calls[name]),
        {
          ...lastCalls,
          items: [...lastCalls.items, ...json.items],
          has_more: json.has_more,
          sort: {
            items:
              lastCalls.sort.column === CALLS_COLUMNS.ID &&
              lastCalls.sort.order === SORT.ASCENDING
                ? [
                  ...lastCalls.sort.items,
                  ...json.items.map(item => ({ ...item, expanded: false })),
                ]
                : sortItems(
                  [
                    ...lastCalls.sort.items,
                    ...json.items.map(item => ({ ...item, expanded: false })),
                  ],
                  lastCalls.sort.column,
                  lastCalls.sort.order,
                ),
            column: lastCalls.sort.column,
            order: lastCalls.sort.order,
          },
        },
      ];
      break;
    default:
      break;
  }
  return callsForFun;
};

export const determineNextControl = (json, lastcalls) => {
  let control;
  switch (callsDecision(json, lastcalls)) {
    case CAPTURE_CALLS_ACTION.APP_INITIALIZATION:
    case CAPTURE_CALLS_ACTION.LAST_CALLS_CAPTURE:
      control = {
        threshold: undefined,
        limit: undefined,
        collecting: false,
      };
      break;
    case CAPTURE_CALLS_ACTION.START_FIRST_CALLS_CAPTURE:
    case CAPTURE_CALLS_ACTION.START_NEXT_CALLS_CAPTURE:
      control = {
        threshold: json.threshold.toString(),
        limit: json.limit.toString(),
        collecting: true,
      };
      break;
    default:
      break;
  }
  return control;
};

export const isStartingNextCapturing = (json, lastcalls) => {
  switch (callsDecision(json, lastcalls)) {
    case CAPTURE_CALLS_ACTION.START_NEXT_CALLS_CAPTURE:
      return true;
    default:
      break;
  }
  return false;
};

export const determineIncomingDps = (dps, ts) => {
  let missingDps;
  let mergedDps = [];
  /* const zeros = {
    min: 0,
    mean: 0,
    median: 0,
    max: 0,
    stddev: 0,
    p25: 0,
    p50: 0,
    p75: 0,
    p90: 0,
    p99: 0,
    p9999999: 0,
    memsize: 0,
    count: 0,
  }; */
  const zeros = {};

  switch (dpsDecision(dps, ts)) {
    case DPS_ACTION.FIRST_DPS:
      missingDps = range(dps[0].time - DPS_LIMIT, dps[0].time).map(time => ({
        ...zeros,
        time,
      }));
      mergedDps = [...missingDps, ...dps];
      break;
    case DPS_ACTION.MISSING_DPS:
      missingDps = range(ts + 1, dps[0].time).map(time => ({
        ...zeros,
        time,
      }));
      mergedDps = [...missingDps, ...dps];
      break;
    case DPS_ACTION.CONTINUOUS_DPS:
      mergedDps = dps;
      break;
    default:
      break;
  }
  return mergedDps.map(sample => ({
    ...sample,
    time: sample.time * 1000,
  }));
};

const isFirstDps = (dps, ts) => dpsDecision(dps, ts) === DPS_ACTION.FIRST_DPS;

export const determineNextData = async (
  dispatch,
  monitoredCollection,
  data,
) => {
  const nextData = {};

  await Promise.all(monitoredCollection.map(async (monitored) => {
    const completeFunName = monitored.query;
    const currentDps = data[completeFunName];
    const lastTs =
        currentDps && currentDps.length ? last(currentDps).time / 1000 : 0;

    const { json, error } = await XProf.getFunctionsSamples(
      monitored.mfa[0],
      monitored.mfa[1],
      monitored.mfa[2],
      lastTs,
    );

    if (error) {
      dispatch(addNotification(
        NOTIFICATIONS.SAMPLES.SEVERITY,
        NOTIFICATIONS.SAMPLES.MESSAGE(monitored.query),
      ));
    } else if (json.length) {
      const incomingDpsSorted = sortBy(json, COLUMNS.time);

      if (isFirstDps(incomingDpsSorted, lastTs)) {
        // eslint-disable-next-line
          const y = Object.keys(incomingDpsSorted[0]).filter(
          f => f !== COLUMNS.time);
        dispatch(addY({ [completeFunName]: y }));
      }

      const incomingDps = determineIncomingDps(incomingDpsSorted, lastTs);
      const concatenatedDps = currentDps
        ? [...currentDps, ...incomingDps]
        : incomingDps;
      const nextDps = takeRight(concatenatedDps, DPS_LIMIT);
      nextData[completeFunName] = nextDps;
    }
  }));

  return nextData;
};

export const determineNextCalls = async (
  dispatch,
  state,
  monitoredCollection,
  calls,
) => {
  const nextCalls = {};

  await Promise.all(monitoredCollection.map(async (monitored) => {
    const completeFunName = monitored.query;
    const lastCalls = getLastCallsForFunction(state, completeFunName);
    const offset =
        lastCalls && lastCalls.items.length ? last(lastCalls.items).id : 0;

    const { json, error } = await XProf.getFunctionsCalls(
      monitored.mfa[0],
      monitored.mfa[1],
      monitored.mfa[2],
      offset,
    );

    if (error) {
      dispatch(addNotification(
        NOTIFICATIONS.CALLS.SEVERITY,
        NOTIFICATIONS.CALLS.MESSAGE(monitored.query),
      ));
    } else {
      const nextControlForFun = determineNextControl(json, lastCalls);
      if (!isEmpty(nextControlForFun)) {
        dispatch(setCallsControl({ [completeFunName]: nextControlForFun }));
      }
      if (isStartingNextCapturing(json, lastCalls)) {
        dispatch(setLastAsCurrentPage(completeFunName));
      }
      const nextCallsForFun = determineNextCallsForFun(
        json,
        lastCalls,
        calls,
        completeFunName,
      );
      if (!isEmpty(nextCallsForFun)) {
        nextCalls[completeFunName] = nextCallsForFun;
      }
    }
  }));

  return nextCalls;
};
export const determineNextControlSwitch = async (
  dispatch,
  control,
  monitored,
) => {
  const { threshold, limit, collecting } = control;
  const nextControl = { ...control };

  if (collecting) {
    const { error } = await XProf.stopCapturingFunctionsCalls(
      monitored.mfa[0],
      monitored.mfa[1],
      monitored.mfa[2],
    );

    if (error) {
      dispatch(addNotification(
        NOTIFICATIONS.STOP_CAPTURING_CALLS.SEVERITY,
        NOTIFICATIONS.START_CAPTURING_CALLS.MESSAGE(monitored.query),
      ));
    }
    nextControl.collecting = false;
  } else {
    const { error } = await XProf.startCapturingFunctionsCalls(
      monitored.mfa[0],
      monitored.mfa[1],
      monitored.mfa[2],
      threshold,
      limit,
    );

    if (error) {
      dispatch(addNotification(
        NOTIFICATIONS.START_CAPTURING_CALLS.SEVERITY,
        NOTIFICATIONS.START_CAPTURING_CALLS.MESSAGE(monitored.query),
      ));
    }
    nextControl.collecting = true;
  }

  return nextControl;
};
