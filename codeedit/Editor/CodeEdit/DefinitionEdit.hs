{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make, makeParts) where

import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, TWidget)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Change parameter name"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Stop changing name"
  }

makeNameEdit :: MonadF m => Widget.Id -> Guid -> TWidget t m
makeNameEdit myId ident =
  BWidgets.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (BWidgets.setTextColor Config.definitionColor .
   BWidgets.makeNameEdit "<unnamed>" ident)
  myId

makeLHSEdit
  :: MonadF m
  => ExpressionGui.Maker m
  -> Widget.Id
  -> Guid
  -> Maybe (Transaction ViewTag m Guid)
  -> (E.Doc, Sugar.ExpressionRef m)
  -> [Sugar.FuncParam m]
  -> OTransaction ViewTag m (ExpressionGui m)
makeLHSEdit makeExpressionEdit myId ident mAddFirstParameter rhs params = do
  nameEdit <-
    liftM (FuncEdit.addJumpToRHS rhs . Widget.weakerEvents addFirstParamEventMap) $
    makeNameEdit myId ident
  liftM (ExpressionGui.hboxSpaced . (ExpressionGui.fromValueWidget nameEdit :)) .
    mapM (FuncEdit.makeParamEdit makeExpressionEdit rhs) $ params
  where
    addFirstParamEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys
       "Add parameter" .
       liftM (FocusDelegator.delegatingId . WidgetIds.paramId) .
       IT.transaction)
      mAddFirstParameter

makeParts
  :: MonadF m
  => ExpressionGui.Maker m
  -> Widget.Id
  -> Guid
  -> Sugar.ExpressionRef m
  -> OTransaction ViewTag m [ExpressionGui m]
makeParts makeExpressionEdit myId guid exprRef = do
  let
    sExpr = Sugar.rExpression exprRef
    func =
      case sExpr of
      Sugar.ExpressionFunc _ x -> x
      _ -> Sugar.Func [] exprRef
  lhsEdit <-
    makeLHSEdit makeExpressionEdit myId guid
    ((fmap Sugar.lambdaWrap . Sugar.eActions . Sugar.rEntity) exprRef)
    ("Def Body", Sugar.fBody func) $ Sugar.fParams func
  equals <- BWidgets.makeLabel "=" $ Widget.toAnimId myId
  let
    lhs = myId : map (WidgetIds.paramId . Sugar.guid . Sugar.fpEntity) (Sugar.fParams func)
  rhsEdit <-
    FuncEdit.makeBodyEdit makeExpressionEdit lhs $ Sugar.fBody func
  return
    [ lhsEdit
    , ExpressionGui.fromValueWidget BWidgets.spaceWidget
    , ExpressionGui.fromValueWidget equals
    , ExpressionGui.fromValueWidget BWidgets.spaceWidget
    , rhsEdit
    ]

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.DefinitionRef m
  -> TWidget ViewTag m
make makeExpressionEdit def = do
  bodyWidget <-
    liftM (ExpressionGui.egWidget . ExpressionGui.hbox) .
    makeParts makeExpressionEdit (WidgetIds.fromGuid guid) guid $
    Sugar.drBody def
  let
    mkResult typeWidget =
      BWidgets.vboxCentered
      [ Widget.scale Config.defTypeBoxSizeFactor typeWidget
      , bodyWidget
      ]
  case Sugar.drMAcceptInferredType def of
    Nothing ->
      if Sugar.drIsTypeRedundant def
      then return bodyWidget
      else liftM (mkResult . BWidgets.hboxSpaced) mkAcceptedWidgets
    Just (Sugar.DefinitionTypeAction inferredType _) -> do
      inferredLabel <-
        labelStyle $ BWidgets.makeLabel "Inferred type:" $ Widget.toAnimId myId
      inferredTypeWidget <- liftM ExpressionGui.egWidget $ makeExpressionEdit inferredType
      acceptedWidgets <- mkAcceptedWidgets
      return . mkResult $
        BWidgets.gridHSpaced
        [acceptedWidgets, [(right, inferredLabel), (center, inferredTypeWidget)]]
  where
    right = Vector2 1 0.5
    center = 0.5
    mkAcceptedWidgets = do
      acceptedLabel <-
        labelStyle $ BWidgets.makeLabel "Type:" $ Widget.toAnimId myId
      acceptedTypeWidget <- liftM ExpressionGui.egWidget . makeExpressionEdit $ Sugar.drType def
      return [(right, acceptedLabel), (center, acceptedTypeWidget)]
    guid = Sugar.drGuid def
    myId = WidgetIds.fromGuid guid
    labelStyle = OT.setTextSizeColor Config.defTypeLabelTextSize Config.defTypeLabelColor
