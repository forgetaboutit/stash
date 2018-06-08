module Core where

import Types

applyCommand :: AppState -> Command -> AppState
applyCommand state (UserCommand uc) = applyUserCommand state uc
applyCommand state (PositionCommand pc) = applyPositionCommand state pc

applyUserCommand :: AppState -> UserCommand -> AppState
applyUserCommand = undefined

applyPositionCommand :: AppState -> PositionCommand -> AppState
applyPositionCommand = undefined
