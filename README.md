# TranalyzerMACO: Trade Analyzer - Moving Average Cross-Over

## Overview
The purpose of this project is to develop a proof-of-concept (PoC) data product in R that can be used to quickly backtest a variety of stock swing trading strategies that require holding a position for multiple days.  Unlike Day Trading (DT) which is practiced by entering and exiting multiple position in a single day, Swing Trading (ST) typically involves holding positions for days, weeks, or sometimes longer.  Because interday quote data is not manditory to practice ST, the use of freely available daily quote data from sources such as **finance.yahoo** can be used.

Each strategy to be evaluated by the TA requires writing three components.  The first component is a **signal generator**.  The second component is the **action generator** which takes the signals from the signal generator and creates recommended actions such as: *buy*, *sell*, or *hold*.  The third component is the **simulator** which takes the actions from the action generator, a position sizing strategy, and an initial account balance as input and outputs positions and account balances on each day of the data set.

The app is hosted at [https://michael-szczepaniak.shinyapps.io/tranmaco/](https://michael-szczepaniak.shinyapps.io/tranmaco/)

## Architecture
The TA is architected to be modular in that to implement a new strategy, you only need to write a new stratgey which conforms to the interface.

### Writing a New Strategy
TBD

## Data
In the demo version, quote data is read from csv files that a local to the application.  There is code in the source that will fetch data from the finance.yahoo.com quote service, but this is bypassed in the demo because of intermittent issues with pulling real-time data.

## Roadmap & Proposed Future Work
#### Support Resistance
#### Bollinger Bands
#### MACD

## Crazy Ideas...
#### Neural Net Classification-based strategies
#### Port to OpenCPU
#### Port to Java as REST service on backend and AngularJS-based client