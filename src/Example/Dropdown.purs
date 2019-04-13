module Example.Dropdown where

import Prelude

import Control.MonadPlus (guard)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

type Query = Const Void

data Action
  = OnInput String
  | HandleDropdown (Select.Message ExtraMessage)

type State =
  { value :: String
  }

type Slots =
  ( dropdown :: Select.Slot Query () ExtraMessage Unit
  )

_dropdown = SProxy :: SProxy "dropdown"

type HTML = H.ComponentHTML Action Slots Aff

initialState :: State
initialState =
  { value: ""
  }

renderSelect :: State -> Select.State () -> H.ComponentHTML (Select.Action () Action) () Aff
renderSelect state st =
  HH.div
  ( Setters.setContainerProps [])
  $ join
  [ pure $ HH.div
    ( Setters.setToggleProps st [])
    [ HH.text "toggle" ]
  , guard (st.visibility == Select.On) $> HH.div_
    [ HH.input
      [ HP.value state.value
      , HE.onValueInput $ Just <<< Select.Action <<< OnInput
      ]
    , HH.div_
      [ HH.text $ "You typed: " <> state.value
      ]
    ]
  ]

data ExtraMessage = Emit Action

render :: State -> HTML
render state =
  HH.div_
  [ HH.slot _dropdown unit (Select.component spec) input
      $ Just <<< HandleDropdown
  ]
  where
  spec :: Select.Spec () Query Action () ExtraMessage Aff
  spec = Select.defaultSpec
    { render = renderSelect state
    , handleAction = H.raise <<< Select.Message <<< Emit
    }
  input =
    { inputType: Select.Toggle
    , debounceTime: Nothing
    , search: Nothing
    , lastIndex: 2
    , watchInput: false
    }

component :: H.Component HH.HTML Query Unit Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction (OnInput value) = do
  H.modify_ $ _ { value = value }
handleAction (HandleDropdown msg) = do
  case msg of
    Select.Message (Emit q) -> handleAction q
    _ -> pure unit
