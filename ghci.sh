#! /usr/bin/env bash
stack ghci \
  "SmallFacts-Core:lib" "SmallFacts-Core:SmallFacts-Core-test" \
  "SmallFacts-CmdLine:lib" "SmallFacts-CmdLine:SmallFacts-CmdLine-test" \
  "SmallFacts:lib" --main-is "SmallFacts:SmallFacts-test"
