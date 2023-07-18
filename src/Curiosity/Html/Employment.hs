{- |
Module: Curiosity.Html.Employment
Description: Employment contract pages (view and edit).
-}
module Curiosity.Html.Employment
  ( ContractView(..)
  , CreateContractPage(..)
  , AddExpensePage(..)
  , RemoveExpensePage(..)
  , ConfirmContractPage(..)
  ) where

import qualified Curiosity.Types.Employment    as Employment
import qualified Curiosity.Types.User          as User
import           Curiosity.Html.Misc
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Panel               ( Panel(..) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!), Html )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data ContractView = ContractView
  { _contractViewContract      :: Employment.Contract
  , _contractViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup ContractView where
  toMarkup (ContractView contract hasEditButton) =
    renderView $ contractView contract hasEditButton

contractView contract hasEditButton = containerLarge $ do
  title' "Employment contract" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID" (Employment._contractId contract)


--------------------------------------------------------------------------------
data CreateContractPage = CreateContractPage
  { _createContractPageUserProfile   :: User.UserProfile
    -- ^ The user creating the contract
  , _createContractPageKey           :: Maybe Text
    -- ^ The form editing session key, if the form was already saved
  , _createContractContract          :: Employment.CreateContractAll
    -- ^ The contract being edited
  , _createContractPageSaveURL       :: H.AttributeValue
  , _createContractPageAddExpenseURL :: H.AttributeValue
  }

instance H.ToMarkup CreateContractPage where
  toMarkup (CreateContractPage profile mkey (Employment.CreateContractAll Employment.CreateContractGenInfo {..} Employment.CreateContractType {} Employment.CreateContractLocDates{} Employment.CreateContractRisks{} Employment.CreateContractInvoice{} expenses) saveUrl addExpenseUrl)
    = renderFormLarge profile $ do
      title "New employment contract"
      panel "General information" $ do

        -- TODO I guess this should be the real name ?
        formKeyValue "Worker username" username
        Misc.inputSelect_
          "project"
          "Project"
          [("none-selected", "Select a project"), ("a", "A")]
          (Just "Enter an existing project or create new one")
          (if _createContractProject == "none-selected"
            then Nothing
            else Just _createContractProject
          )
          True
        inputText "Purchase order"
                  "po"
                  (Just $ H.toValue _createContractPO)
                  Nothing
        inputText "Role" "role" (Just $ H.toValue _createContractRole) Nothing
        inputText "Work type"
                  "type"
                  (Just $ H.toValue _createContractType)
                  Nothing
        Misc.inputTextarea "description"
                           "Description"
                           6
                           "Describe your work (minimum 10 characters)"
                           _createContractDescription
                           True
      panel "Employment type" $ groupEmployment
      panel "Location and dates" $ do
        inputText "Work country" "country" Nothing Nothing
        inputText "Work dates"   "dates"   Nothing Nothing
      panel "Risks" $ groupRisks
      panel "Invoicing" $ groupInvoicing
      (! A.id "panel-expenses") $ panelStandard "Expenses" $ groupExpenses
        mkey
        expenses
        addExpenseUrl

      groupLayout $ do
        submitButton saveUrl "Save changes"
   where
    username =
      User.unUserName . User._userCredsName $ User._userProfileCreds profile

groupRisks = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" $ "Work risks"
  H.div
    ! A.class_ "o-form-group__controls"
    $ H.div
    ! A.class_ "c-radio-group c-radio-group--inline"
    $ do
        H.div ! A.class_ "c-radio" $ H.label $ do
          H.input ! A.type_ "radio" ! A.name "radio1"
          "Without risks"
        H.div ! A.class_ "c-radio" $ H.label $ do
          H.input ! A.type_ "radio" ! A.name "radio1"
          "With risks"
        H.p ! A.class_ "c-form-help-text" $ do
          "For your safety and your colleagues safety, if your role "
          "involves risks, you have to select \"With risks\". "
          H.a ! A.class_ "c-link" ! A.href "#" $ "Learn more about risks."

groupInvoicing = do
  inputText "Client" "client" Nothing Nothing
  inputText "Amount" "amount" Nothing Nothing
  inputText "VAT"    "vat"    Nothing Nothing
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $ "Include VAT ?"
    H.div
      ! A.class_ "o-form-group__controls"
      $ H.div
      ! A.class_ "c-radio-group c-radio-group--inline"
      $ do
          H.div ! A.class_ "c-radio" $ H.label $ do
            H.input ! A.type_ "radio" ! A.name "radio2"
            "Include"
          H.div ! A.class_ "c-radio" $ H.label $ do
            H.input ! A.type_ "radio" ! A.name "radio2"
            "Exclude"
          H.p
            ! A.class_ "c-form-help-text"
            $ "Usually, business clients prefer to see amounts VAT excluded."
  inputText "Down payment" "down-payment" Nothing Nothing

groupExpenses mkey expenses submitUrl = do
  H.div ! A.class_ "o-form-group" $ do
    when (null expenses)
      $ H.div
      ! A.class_ "c-blank-slate c-blank-slate--bg-alt"
      $ do
          H.p
            ! A.class_ "u-text-muted c-body-1"
            $ "When using a project, you can choose to add expenses directly or later."
          H.div ! A.class_ "c-button-toolbar" $ buttonAdd submitUrl
                                                          "Add expense"
    case mkey of
      Just key | not (null expenses) ->
        Misc.table "expenses" titles (uncurry $ display key) $ zip [0 ..] expenses
      _ -> pure ()
    when (not $ null expenses) $ buttonGroup $ buttonAdd submitUrl "Add expense"
 where
  titles = ["Amount"]
  display
    :: Text
    -> Int
    -> Employment.AddExpense
    -> ([Text], [(Html, Text, Text)], Maybe Text)
  display key i Employment.AddExpense {..} =
    ( [show _addExpenseAmount]
    , [ ( Misc.divIconDelete
        , "Remove"
        , "/forms/remove-expense/" <> key <> "/" <> show i
        )
      ]
    , (Just $ "/forms/edit-expense/" <> key <> "/" <> show i)
    )

groupEmployment = do
  inputText "Withholding tax"     "withholding-tax" Nothing Nothing
  inputText "Student / au cachet" "TODO"            Nothing Nothing
  inputText "Interim"             "interim"         Nothing Nothing

formKeyValue :: Text -> Text -> Html
formKeyValue label value = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" $ H.text label
  H.div
    ! A.class_ "o-form-group__controls o-form-group__controls--full-width"
    $ H.label
    $ H.text value


--------------------------------------------------------------------------------
data AddExpensePage = AddExpensePage
  { _addExpensePageUserProfile :: User.UserProfile
    -- ^ The user creating the expense within a contract
  , _addExpensePageKey         :: Text
    -- ^ The key of the contract form
  , _addExpensePageIndex       :: Maybe Int
    -- ^ The index of the expense within CreateContractAll, if it was already
    -- added
  , _addExpensePageExpense     :: Employment.AddExpense
  , _addExpensePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup AddExpensePage where
  toMarkup (AddExpensePage profile key mindex expense submitUrl) =
    renderFormLarge profile $ do
      title' label Nothing

      panel "General information" $ do
        H.input
          ! A.type_ "hidden"
          ! A.id "contract-key"
          ! A.name "contract-key"
          ! A.value (H.toValue key)
        inputText "Amount" "amount" mamount Nothing

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      buttonBar $ do
        buttonLink
          (H.toValue $ "/forms/edit-contract/" <> key <> "#panel-expenses")
          "Cancel"
        buttonPrimary submitUrl label
   where
    label   = if isJust mindex then "Update expense" else "Add expense"
    mamount = if amount /= 0 then Just (H.toValue amount) else Nothing
    amount  = Employment._addExpenseAmount expense


--------------------------------------------------------------------------------
data RemoveExpensePage = RemoveExpensePage
  { _removeExpensePageUserProfile :: User.UserProfile
    -- ^ The user creating the expense within a contract
  , _removeExpensePageKey         :: Text
    -- ^ The key of the contract form
  , _removeExpensePageIndex       :: Int
    -- ^ The index of the expense within CreateContractAll
  , _removeExpensePageExpense     :: Employment.AddExpense
  , _removeExpensePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup RemoveExpensePage where
  toMarkup (RemoveExpensePage profile _ _ expense submitUrl) =
    renderFormLarge profile $ do
      title' "Remove expense" Nothing

      panel "General information" $ do
        keyValuePair "Amount" mamount

      button submitUrl "Remove expense"
   where
    mamount = if amount /= 0 then show amount else "/" :: Text
    amount  = Employment._addExpenseAmount expense


--------------------------------------------------------------------------------
data ConfirmContractPage = ConfirmContractPage
  { _confirmContractPageUserProfile :: User.UserProfile
    -- ^ The user creating the contract
  , _confirmContractPageKey         :: Text
  , _confirmContractPageContract    :: Employment.CreateContractAll
  , _confirmContractPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmContractPage where
  toMarkup (ConfirmContractPage profile key (Employment.CreateContractAll Employment.CreateContractGenInfo {..} Employment.CreateContractType {} Employment.CreateContractLocDates{} Employment.CreateContractRisks{} Employment.CreateContractInvoice{} expenses) submitUrl)
    = renderFormLarge profile $ do
      title' "New employment contract"
        .  Just
        .  H.toValue
        $  "/forms/edit-contract/"
        <> key

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "General information"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "Worker username" username
            keyValuePair "Project"         _createContractProject
            keyValuePair "Purchase order"  _createContractPO
            keyValuePair "Role"            _createContractRole
            keyValuePair "Work type"       _createContractType
            keyValuePair "Description"     _createContractDescription

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Expenses"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ if null expenses
            then H.div ! A.class_ "c-blank-slate c-blank-slate--bg-alt" $ do
              H.p
                ! A.class_ "u-text-muted c-body-1"
                $ "You have no expenses right now."
            else Misc.table "expenses" titles display expenses

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      button submitUrl "Submit contract"
   where
    username =
      User.unUserName . User._userCredsName $ User._userProfileCreds profile
    titles = ["Amount"]
    display
      :: Employment.AddExpense
      -> ([Text], [(Html, Text, Text)], Maybe Text)
    display Employment.AddExpense {..} =
      ([show _addExpenseAmount], [], Nothing)
