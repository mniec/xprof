import PropTypes from 'prop-types';
import React from 'react';
import { FunctionBrowser, TracingSwitch, GridSwitch } from '../';
import logo from './logo.png';
import { PLACEHOLDER, INPUT_TYPE } from '../../../constants';

const defaultProps = {
  position: -1,
  language: null,
  inputType: null,
  example: null,
  numberOfMonitoredFunctions: 0,
  selectedInputType: INPUT_TYPE.SEARCH,
  favourites: [],
};

const propTypes = {
  status: PropTypes.string.isRequired,
  toggleTraceStatus: PropTypes.func.isRequired,
  queryKeyDown: PropTypes.func.isRequired,
  queryInputChange: PropTypes.func.isRequired,
  query: PropTypes.string.isRequired,
  functions: PropTypes.arrayOf(PropTypes.object).isRequired,
  functionClick: PropTypes.func.isRequired,
  position: PropTypes.number,
  setPositionOnFunction: PropTypes.func.isRequired,
  language: PropTypes.string,
  inputType: PropTypes.string,
  example: PropTypes.string,
  isConnection: PropTypes.bool.isRequired,
  switchGrid: PropTypes.func.isRequired,
  numberOfMonitoredFunctions: PropTypes.number,
  selectedInputType: PropTypes.string,
  switchInputType: PropTypes.func.isRequired,
  toggleInputType: PropTypes.func.isRequired,
  favourites: PropTypes.arrayOf(PropTypes.string),
};

const determinePlaceholder = (
  language,
  inputType,
  example,
  selectedInputType,
  favourites,
) => {
  if (selectedInputType === INPUT_TYPE.SEARCH) {
    if (language && inputType && example) {
      return PLACEHOLDER.MODE_DETECTED(language, inputType, example);
    }
    return PLACEHOLDER.MODE_UNKNOWN;
  } else if (selectedInputType === INPUT_TYPE.FAVOURITES) {
    if (favourites.length) {
      return PLACEHOLDER.HAVE_FAVOURITES;
    }
    return PLACEHOLDER.DONT_HAVE_FAVOURITES;
  }
  return '';
};

const Navbar = ({
  status,
  toggleTraceStatus,
  queryKeyDown,
  queryInputChange,
  query,
  functions,
  functionClick,
  position,
  setPositionOnFunction,
  language,
  inputType,
  example,
  isConnection,
  switchGrid,
  numberOfMonitoredFunctions,
  selectedInputType,
  switchInputType,
  toggleInputType,
  favourites,
}) => (
  <nav className="navbar navbar-default navbar-fixed-top">
    <div className="navbar-header">
      <a className="navbar-brand" href="#top">
        <img src={logo} alt="XProf logo" height="45px" />
      </a>
    </div>
    <div className="navbar-collapse collapse" id="navbar-collapsible">
      <TracingSwitch
        status={status}
        toggleTraceStatus={toggleTraceStatus}
        isConnection={isConnection}
      />
      <GridSwitch
        onChange={switchGrid}
        disabled={numberOfMonitoredFunctions < 2}
      />
      <FunctionBrowser
        queryKeyDown={queryKeyDown}
        queryInputChange={queryInputChange}
        query={query}
        placeholder={determinePlaceholder(
          language,
          inputType,
          example,
          selectedInputType,
          favourites,
        )}
        functions={functions}
        functionClick={functionClick}
        position={position}
        setPositionOnFunction={setPositionOnFunction}
        isConnection={isConnection}
        selectedInputType={selectedInputType}
        switchInputType={switchInputType}
        toggleInputType={toggleInputType}
      />
    </div>
  </nav>
);

Navbar.defaultProps = defaultProps;
Navbar.propTypes = propTypes;

export default Navbar;
