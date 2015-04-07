{-# LANGUAGE OverloadedStrings #-}
-- |

module React.Bootstrap where

import React

container m =
  build "div"
        (do attr "className" "container"
            m)

row m =
  build "div"
        (do attr "className" "row"
            m)

span12 m =
  build "div"
        (do attr "className" "span12"
            m)
