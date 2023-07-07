{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{- |
Module: Curiosity.Html.SimpleContract
Description: Employment contract pages (view and edit) for the "simple" (also
called 3in1) contract flow.
-}
module Curiosity.Html.SimpleContract
  ( SimpleContractView(..)
  , CreateSimpleContractPage(..)
  , SelectRolePage(..)
  , ConfirmRolePage(..)
  , AddDatePage(..)
  , RemoveDatePage(..)
  , SelectVATPage(..)
  , ConfirmVATPage(..)
  , SimpleContractAddExpensePage(..)
  , SimpleContractRemoveExpensePage(..)
  , ConfirmSimpleContractPage(..)
  ) where

import qualified Curiosity.Data.Country        as Country
import qualified Curiosity.Data.SimpleContract as SimpleContract
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import qualified Smart.Html.Alert              as Alert
import qualified Smart.Html.Button             as Button
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Panel               ( Panel(..) )
import           Smart.Html.Shared.Html.Icons
import           Smart.Html.Shared.Types        ( Body(..) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!), Html )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data SimpleContractView = SimpleContractView
  { _contractViewSimpleContract :: SimpleContract.Contract
  , _contractViewHasEditButton  :: Maybe H.AttributeValue
  }

instance H.ToMarkup SimpleContractView where
  toMarkup (SimpleContractView contract hasEditButton) =
    renderView $ contractView contract hasEditButton

contractView contract hasEditButton = containerLarge $ do
  title' "Employment contract" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID" (SimpleContract._contractId contract)


--------------------------------------------------------------------------------
data CreateSimpleContractPage = CreateSimpleContractPage
  { _createSimpleContractPageUserProfile   :: User.UserProfile
    -- ^ The user creating the contract
  , _createSimpleContractPageKey           :: Maybe Text
    -- ^ The form editing session key, if the form was already saved
  , _createSimpleContractContract          :: SimpleContract.CreateContractAll
    -- ^ The contract being edited
  , _createSimpleContractRoleLabel         :: Text
    -- ^ The human text for the role, already looked un in `roles'`.
  , _createSimpleContractPageSaveURL       :: H.AttributeValue
  , _createSimpleContractPageSelectRoleURL :: H.AttributeValue
  , _createSimpleContractPageAddDateURL    :: H.AttributeValue
  , _createSimpleContractPageEditDateURL   :: Text
  , _createSimpleContractPageRemoveDateBaseURL :: Text
  , _createSimpleContractPageSelectVATURL  :: H.AttributeValue
  , _createSimpleContractPageAddExpenseURL :: H.AttributeValue
  , _createSimpleContractPageEditExpenseBaseURL :: Text
  , _createSimpleContractPageRemoveExpenseBaseURL :: Text
  }

instance H.ToMarkup CreateSimpleContractPage where
  toMarkup (CreateSimpleContractPage profile mkey contract roleLabel saveUrl selectRoleUrl addDateUrl editDateBaseUrl removeDateBaseUrl selectVATUrl addExpenseUrl editExpenseBaseUrl removeExpenseBaseUrl)
    = renderFormLarge profile $ do
      title "New simple contract"

      information

      (! A.id "panel-type") $ panel "Service type" $ groupType contract
                                                               roleLabel
                                                               selectRoleUrl
      panel "Risks" $ groupRisks
      (! A.id "panel-dates") $ panelStandard "Work dates" $ groupDates
        editDateBaseUrl
        removeDateBaseUrl
        mkey
        dates
        addDateUrl
      (! A.id "panel-client") $ panel "Client" $ groupClient client
      (! A.id "panel-invoicing") $ panel "Invoicing and contract type" $ groupInvoicing inv selectVATUrl
      (! A.id "panel-expenses") $ panelStandard "Expenses" $ groupExpenses
        editExpenseBaseUrl
        removeExpenseBaseUrl
        mkey
        expenses
        addExpenseUrl

      groupLayout $ do
        submitButton saveUrl "Save changes"
   where
    dates = SimpleContract._createContractDates contract
    client = SimpleContract._createContractClient contract
    inv = SimpleContract._createContractInvoice contract
    expenses = SimpleContract._createContractExpenses contract

information =
  H.div ! A.class_ "u-spacer-bottom-l" $ H.toMarkup $ Alert.Alert
    Alert.AlertDefault
    iconInformation
    "Message about the 3 simple contracts, possible without social shares."
    Button.NoButton

iconInformation =
  Just $ OSvgIconDiv @"circle-information" svgIconCircleInformation

groupType SimpleContract.CreateContractAll {..} roleLabel selectRoleUrl = do
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for "worker" $ "Role"
    H.div
      ! A.class_ "o-form-group__controls o-form-group__controls--full-width"
      $ do
          H.div
            ! A.class_ "o-flex o-flex--vertical-center u-maximize-width"
            $ do
                H.span ! A.class_ "u-spacer-right-s" $ H.text roleLabel
                H.button
                  ! A.class_ "c-button c-button--borderless c-button--icon"
                  ! A.formaction selectRoleUrl
                  ! A.formmethod "POST"
                  $ H.span
                  ! A.class_ "c-button__content"
                  $ do
                      divIconEdit
                      H.div ! A.class_ "u-sr-accessible" $ "Edit"
          H.p
            ! A.class_ "c-form-help-text"
            $ "Your most frequently used role has been automatically selected. Click the edit button if you want to select a different role for this work."

  H.input ! A.type_ "hidden" ! A.id "role" ! A.name "role" ! A.value
    (H.toValue $ SimpleContract._createContractRole _createContractType)
  Misc.inputTextarea
    "description"
    "Description"
    6
    "A description of your work. This will appear on the invoice sent to the client. (Minimum 10 characters.)"
    (SimpleContract._createContractDescription _createContractType)
    True
  Misc.inputSelect_
    "work-country"
    "Work country"
    Country.countries
    Nothing
    (Just $ SimpleContract._createContractWorkCountry _createContractType)
    False
  inputRisks $ SimpleContract._createContractHasRisks _createContractType

-- TODO Same in Employment.Contract.
inputRisks hasRisks = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" $ "Work risks"
  H.div
    ! A.class_ "o-form-group__controls"
    $ H.div
    ! A.class_ "c-radio-group c-radio-group--inline"
    $ do
        H.div ! A.class_ "c-radio" $ H.label $ do
          (if not hasRisks then (! A.checked "checked") else identity) $
            H.input ! A.type_ "radio" ! A.name "has-risks" ! A.value "False"
          "Without risks"
        H.div ! A.class_ "c-radio" $ H.label $ do
          (if hasRisks then (! A.checked "checked") else identity) $
            H.input ! A.type_ "radio" ! A.name "has-risks" ! A.value "True"
          "With risks"
        H.p ! A.class_ "c-form-help-text" $ do
          "For your safety and your colleagues safety, if your role "
          "involves risks, you have to select \"With risks\". "
          H.a ! A.class_ "c-link" ! A.href "#" $ "Learn more about risks."

groupRisks = H.div ! A.class_ "o-form-group" $ do
  H.toMarkup $ Alert.AlertLight
    Alert.AlertDefault
    iconInformation
    "Fill this panel only when you selected \"With risks\" above."
    Button.NoButton

-- TODO This is the same editing pattern as groupExpenses: start with an empty
-- list, then add / delete / edit items. This should be abstracted away.
groupDates editDateBaseUrl removeDateBaseUrl mkey dates submitUrl = do
  H.div ! A.class_ "o-form-group" $ do
    when (null dates)
      $ H.div
      ! A.class_ "c-blank-slate c-blank-slate--bg-alt"
      $ do
          H.p
            ! A.class_ "u-text-muted c-body-1"
            $ "Select the dates when you'll be working. All the dates must be within the same month."
          H.div ! A.class_ "c-button-toolbar" $ buttonAdd submitUrl
                                                          "Add date"
    case mkey of
      Just key | not (null dates) ->
        Misc.table "dates" titles (uncurry $ display key) $ zip [0 ..] dates
      _ -> pure ()
    when (not $ null dates) $ buttonGroup $ buttonAdd submitUrl "Add date"
 where
  titles = ["Date"]
  display
    :: Text
    -> Int
    -> SimpleContract.AddDate
    -> ([Text], [(Html, Text, Text)], Maybe Text)
  display key i SimpleContract.AddDate {..} =
    ( [_addDateDate]
    , [ ( Misc.divIconDelete
        , "Remove"
        , removeDateBaseUrl <> key <> "/" <> show i
        )
      ]
    , (Just $ editDateBaseUrl <> key <> "/" <> show i)
    )

groupClient SimpleContract.CreateContractClient {..} = do
  inputText "Client" "client-username" musername (Just "The client username.")
 where
  musername = (H.toValue . User.unUserName) <$> _createContractClientUsername

groupInvoicing inv selectVATUrl = do
  let vat = SimpleContract._createContractVAT inv
      amount = SimpleContract._createContractAmount inv
      prepaid = SimpleContract._createContractPrepaidAmount inv
      whtax = SimpleContract._createContractWithholdingTax inv
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $
      "Amount"
    H.div ! A.class_ "o-form-group__controls o-form-group__controls--full-width" $ do
      H.div ! A.class_ "o-flex-bp2 o-flex--spaced-wide o-flex--vertical-center" $ do
        H.div ! A.class_ "c-input-group" $ do
          H.div ! A.class_ "c-input-group__input" $
            H.input ! A.class_ "c-input" ! A.type_ "number" ! A.id "amount" ! A.name "amount" ! A.value (H.toValue amount)
          H.div ! A.class_ "c-input-group__append" $ "€"
        H.div ! A.class_ "c-checkbox" $
          H.label $ do
            H.input ! A.type_ "checkbox"
            "Unknown amount"
      H.p ! A.class_ "c-form-help-text" $
        "Enter an amount or select \"Unknown amount\"."
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $
      "VAT rate"
    H.div ! A.class_ "o-form-group__controls o-form-group__controls--full-width" $ do
      H.div ! A.class_ "o-flex-bp2 o-flex--spaced-wide o-flex--vertical-center" $ do
        H.div ! A.class_ "c-input-group" $ do
          H.div ! A.class_ "c-input-group__input" $
            H.input ! A.class_ "c-input" ! A.id "vat" ! A.name "vat" ! A.value (H.toValue vat)
          H.div ! A.class_ "c-input-group__append" $ "%"
        buttonSecondary selectVATUrl "Change VAT"
      H.p ! A.class_ "c-form-help-text" $
        "The normal VAT is 21%. In some cases a reduced rate can be applied using the \"Change VAT rate\" button."
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $ "Include VAT ?"
    H.div
      ! A.class_ "o-form-group__controls"
      $ H.div
      ! A.class_ "c-radio-group c-radio-group--inline"
      $ do
          H.div ! A.class_ "c-radio" $ H.label $ do
            H.input ! A.type_ "radio" ! A.name "vat-incl-excl" ! A.value "True"
            "VAT Included"
          H.div ! A.class_ "c-radio" $ H.label $ do
            H.input ! A.type_ "radio" ! A.name "vat-incl-excl" ! A.value "False"
            "VAT Excluded"
          H.p
            ! A.class_ "c-form-help-text"
            $ "Specify if the above amount includes VAT or not."
  inputText "Prepaid amount" "prepaid-amount" (Just $ H.toValue prepaid) Nothing
  inputText "Withholding tax"     "withholding-tax" (Just $ H.toValue whtax) Nothing
  Misc.inputSelect_
    "contract-type"
    "Contract type"
    [ ("none-selected", "Please select a contract type")
    , ("normal", "Duration-based work contract")
    , ("artistic", "Task-based artistic contract")
    , ("artistic-1bis", "1st bis artistic contract")
    , ("student", "Student work contract")
    , ("interim", "Interim contract")
    ]
    Nothing
    Nothing
    False

groupExpenses editExpenseBaseUrl removeExpenseBaseUrl mkey expenses submitUrl = do
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
    -> SimpleContract.AddExpense
    -> ([Text], [(Html, Text, Text)], Maybe Text)
  display key i SimpleContract.AddExpense {..} =
    ( [show _addExpenseAmount]
    , [ ( Misc.divIconDelete
        , "Remove"
        , removeExpenseBaseUrl <> key <> "/" <> show i
        )
      ]
    , (Just $ editExpenseBaseUrl <> key <> "/" <> show i)
    )


--------------------------------------------------------------------------------
data SelectRolePage = SelectRolePage
  { _selectRolePageUserProfile        :: User.UserProfile
    -- ^ The user creating the simple contract
  , _selectRolePageKey                :: Text
    -- ^ The key of the contract form
  , _selectRolePageConfirmRoleBaseURL :: H.AttributeValue
  }

instance H.ToMarkup SelectRolePage where
  toMarkup (SelectRolePage profile key confirmRoleBaseUrl) = renderFormLarge profile $ do
    title' "Select role" Nothing

    H.div ! A.class_ "c-content" $ do
      mapM_ (displayRole0 confirmRoleBaseUrl key) SimpleContract.roles

displayRole0 confirmRoleBaseUrl key (SimpleContract.Role0 title_ roles1) = do
  H.h3 $ H.text title_
  mapM_ (displayRole1 confirmRoleBaseUrl key) roles1

displayRole1 confirmRoleBaseUrl key (SimpleContract.Role1 title_ roles) = do
  H.h4 $ H.text title_
  H.ul $ mapM_ (displayRole confirmRoleBaseUrl key) roles

displayRole confirmRoleBaseUrl key (value, label) =
  H.li
    $ H.a
    ! A.href (confirmRoleBaseUrl <> H.toValue (key <> "/" <> value))
    $ H.text label


--------------------------------------------------------------------------------
data ConfirmRolePage = ConfirmRolePage
  { _confirmRolePageUserProfile :: User.UserProfile
    -- ^ The user creating the simple contract
  , _confirmRolePageKey         :: Text
    -- ^ The key of the contract form
  , _confirmRolePageRole        :: Text
    -- ^ The role being selected
  , _confirmRolePageLabel       :: Text
    -- ^ The human text for the role, already looked up in `roles'`.
  , _confirmRolePageEditURL     :: H.AttributeValue
  , _confirmRolePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmRolePage where
  toMarkup (ConfirmRolePage profile key role label editUrl submitUrl) =
    renderFormLarge profile $ do
      H.toMarkup
        $ PanelHeaderAndBody "Confirm selected role"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "Role" label
            H.input ! A.type_ "hidden" ! A.id "role" ! A.name "role" ! A.value
              (H.toValue role)

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)

      buttonBar $ do
        buttonLink editUrl "Cancel"
        buttonPrimary submitUrl "Select role"


--------------------------------------------------------------------------------
data AddDatePage = AddDatePage
  { _addDatePageUserProfile :: User.UserProfile
    -- ^ The user creating the date within a contract
  , _addDatePageKey         :: Text
    -- ^ The key of the contract form
  , _addDatePageIndex       :: Maybe Int
    -- ^ The index of the date within CreateContractAll, if it was already
    -- added
  , _addDatePageDate        :: SimpleContract.AddDate
  , _addDatePageEditURL     :: H.AttributeValue
  , _addDatePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup AddDatePage where
  toMarkup (AddDatePage profile key mindex date editUrl submitUrl) =
    renderFormLarge profile $ do
      title' label Nothing

      panel "Work dates" $ do
        inputText "Date" "date" mamount Nothing

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      buttonBar $ do
        buttonLink editUrl "Cancel"
        buttonPrimary submitUrl label
   where
    label   = if isJust mindex then "Update date" else "Add date"
    mamount = if amount /= "1970-01-01" then Just (H.toValue amount) else Nothing
    amount  = SimpleContract._addDateDate date


--------------------------------------------------------------------------------
data RemoveDatePage = RemoveDatePage
  { _removeDatePageUserProfile :: User.UserProfile
    -- ^ The user creating the date within a contract
  , _removeDatePageKey         :: Text
    -- ^ The key of the contract form
  , _removeDatePageIndex       :: Int
    -- ^ The index of the date within CreateContractAll
  , _removeDatePageDate     :: SimpleContract.AddDate
  , _removeDatePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup RemoveDatePage where
  toMarkup (RemoveDatePage profile _ _ date submitUrl) =
    renderFormLarge profile $ do
      title' "Remove date" Nothing

      panel "Work dates" $ do
        keyValuePair "Date" mamount

      button submitUrl "Remove date"
   where
    mamount = if amount /= "1970-01-01" then amount else "/" :: Text
    amount  = SimpleContract._addDateDate date


--------------------------------------------------------------------------------
data SelectVATPage = SelectVATPage
  { _selectVATPageUserProfile       :: User.UserProfile
    -- ^ The user creating the simple contract
  , _selectVATPageKey               :: Text
    -- ^ The key of the contract form
  , _selectVATPageConfirmVatBaseUrl :: H.AttributeValue
  }

instance H.ToMarkup SelectVATPage where
  toMarkup (SelectVATPage profile key confirmVatBaseUrl) = renderFormLarge profile $ do
    title' "Select VAT" Nothing

    H.ul $ mapM_ (displayRate confirmVatBaseUrl key) SimpleContract.vatRates

displayRate confirmVatBaseUrl key (value, label) =
  H.li
    $ H.a
    ! A.href (confirmVatBaseUrl <> H.toValue ( key <> "/" <> value))
    $ H.text label

--------------------------------------------------------------------------------
data ConfirmVATPage = ConfirmVATPage
  { _confirmVATPageUserProfile :: User.UserProfile
    -- ^ The user creating the simple contract
  , _confirmVATPageKey         :: Text
    -- ^ The key of the contract form
  , _confirmVATPageRate        :: Int
    -- ^ The rate being selected
  , _confirmVATPageEditURL     :: H.AttributeValue
  , _confirmVATPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmVATPage where
  toMarkup (ConfirmVATPage profile key rate editUrl submitUrl) =
    renderFormLarge profile $ do
      H.toMarkup
        $ PanelHeaderAndBody "Confirm selected VAT"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "VAT" (show rate <> "%" :: Text)
            H.input ! A.type_ "hidden" ! A.id "vat" ! A.name "vat" ! A.value
              (H.toValue rate)

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)

      buttonBar $ do
        buttonLink editUrl "Cancel"
        buttonPrimary submitUrl "Select VAT"


--------------------------------------------------------------------------------
data SimpleContractAddExpensePage = SimpleContractAddExpensePage
  { _addExpensePageUserProfile :: User.UserProfile
    -- ^ The user creating the expense within a contract
  , _addExpensePageKey         :: Text
    -- ^ The key of the contract form
  , _addExpensePageIndex       :: Maybe Int
    -- ^ The index of the expense within CreateContractAll, if it was already
    -- added
  , _addExpensePageExpense     :: SimpleContract.AddExpense
  , _addExpensePageEditURL     :: H.AttributeValue
  , _addExpensePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup SimpleContractAddExpensePage where
  toMarkup (SimpleContractAddExpensePage profile key mindex expense editUrl submitUrl) =
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
        buttonLink editUrl "Cancel"
        buttonPrimary submitUrl label
   where
    label   = if isJust mindex then "Update expense" else "Add expense"
    mamount = if amount /= 0 then Just (H.toValue amount) else Nothing
    amount  = SimpleContract._addExpenseAmount expense


--------------------------------------------------------------------------------
data SimpleContractRemoveExpensePage = SimpleContractRemoveExpensePage
  { _removeExpensePageUserProfile :: User.UserProfile
    -- ^ The user creating the expense within a contract
  , _removeExpensePageKey         :: Text
    -- ^ The key of the contract form
  , _removeExpensePageIndex       :: Int
    -- ^ The index of the expense within CreateContractAll
  , _removeExpensePageExpense     :: SimpleContract.AddExpense
  , _removeExpensePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup SimpleContractRemoveExpensePage where
  toMarkup (SimpleContractRemoveExpensePage profile _ _ expense submitUrl) =
    renderFormLarge profile $ do
      title' "Remove expense" Nothing

      panel "General information" $ do
        keyValuePair "Amount" mamount

      button submitUrl "Remove expense"
   where
    mamount = if amount /= 0 then show amount else "/" :: Text
    amount  = SimpleContract._addExpenseAmount expense


--------------------------------------------------------------------------------
data ConfirmSimpleContractPage = ConfirmSimpleContractPage
  { _confirmSimpleContractPageUserProfile :: User.UserProfile
    -- ^ The user creating the contract
  , _confirmSimpleContractPageKey         :: Text
  , _confirmSimpleContractPageContract    :: SimpleContract.CreateContractAll
  , _confirmSimpleContractPageErrors      :: [SimpleContract.Err]
    -- Validation errors, if any.
  , _confirmSimpleContractPageRoleLabel   :: Text
    -- ^ The human text for the role, already looked up in `roles'`.
  , _confirmSimpleContractPageEditURL     :: Maybe H.AttributeValue
  , _confirmSimpleContractPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmSimpleContractPage where
  toMarkup (ConfirmSimpleContractPage profile key (SimpleContract.CreateContractAll SimpleContract.CreateContractType {..} SimpleContract.CreateContractRisks{} SimpleContract.CreateContractClient {..} SimpleContract.CreateContractInvoice{} dates expenses) errors roleLabel meditUrl submitUrl)
    = renderFormLarge profile $ do
      title' "New simple contract" meditUrl

      validationErrors errors

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Service type"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "Worker username" username
            keyValuePair "Role"            roleLabel
            keyValuePair "Description"     _createContractDescription
            keyValuePair "Work country" $ maybe "TODO" identity $ Country.lookupCountry
              _createContractWorkCountry
            keyValuePair "Work risks"     $ if _createContractHasRisks then "With risks" else "Without risks" :: Text

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Risks"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ H.div ! A.class_ "c-blank-slate c-blank-slate--bg-alt" $ do
            H.p
              ! A.class_ "u-text-muted c-body-1"
              $ ""

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Work dates"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ if null dates
            then H.div ! A.class_ "c-blank-slate c-blank-slate--bg-alt" $ do
              H.p
                ! A.class_ "u-text-muted c-body-1"
                $ "At least one date is required."
            else H.ul $
                   mapM_ (H.li . H.text . SimpleContract._addDateDate) dates

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Client"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ case mClientUsername of
            Nothing ->
              H.div ! A.class_ "c-blank-slate c-blank-slate--bg-alt" $ do
                H.p
                  ! A.class_ "u-text-muted c-body-1"
                  $ "A client must be selected."
            Just clientUsername ->
              keyValuePair "Client username" clientUsername

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Invoicing and contract type"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ "TODO"

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
                $ "You have added no expenses."
            else Misc.table "expenses" titles display expenses

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)

      buttonGroup $ do
        -- TODO Add edit button
        let label = "Submit contract"
        if null errors
          then buttonPrimary submitUrl label
          else buttonPrimaryDisabled label
   where
    username =
      User.unUserName . User._userCredsName $ User._userProfileCreds profile
    mClientUsername = User.unUserName <$> _createContractClientUsername
    titles = ["Amount"]
    display
      :: SimpleContract.AddExpense
      -> ([Text], [(Html, Text, Text)], Maybe Text)
    display SimpleContract.AddExpense {..} =
      ([show _addExpenseAmount], [], Nothing)

validationErrors errors =
  if null errors
  then mempty
  else
    H.div ! A.class_ "u-spacer-bottom-l" $ H.toMarkup $ Alert.Alert
      Alert.AlertError
      iconError
      (Body $ "Validation errors: " <> show (map SimpleContract.unErr errors))
      Button.NoButton
