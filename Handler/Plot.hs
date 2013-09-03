module Handler.Plot where

import Control.Arrow
import qualified Data.Text as T
import Import
import qualified Graphics.EasyPlot as EP
import qualified Data.UUID as U
import qualified Data.UUID.V4 as V4

getPlotFormR :: Handler Html
getPlotFormR = do
    (plotWidget,enctype) <- generateFormPost plotForm
    defaultLayout $ do
        setTitle "Plotter"
        $(widgetFile "plots")

postPlotFormR :: Handler Html
postPlotFormR = do
    ((response,plotWidget),enctype) <- runFormPost plotForm
    case response of
        FormSuccess plot -> do
            newUUID <- liftIO $ V4.nextRandom
            let filename = makeFilename $ U.toString newUUID
            setMessage "Generating plot"
            _ <- ($) liftIO $ makePlot filename $ plotDataData plot
            plotId <- runDB $ insert $ Plot (T.pack filename)
            redirect $ PlotR plotId
        _ -> defaultLayout $ do
            setTitle "Invalid plot data"
            $(widgetFile "plots")

getPlotR :: PlotId -> Handler Html
getPlotR plotId = do
    plot <- runDB $ get404 plotId
    let filename = "/" <> plotFilename plot
    defaultLayout $ do
        setTitle "Plot!!!"
        $(widgetFile "plot")

deletePlotR :: PlotId -> Handler Html
deletePlotR = error "Not yet implemented: deletePlotR"

plotForm :: Html -> MForm (HandlerT App IO)
                          (FormResult PlotData,
                           WidgetT (HandlerSite (HandlerT App IO)) IO ())
plotForm = renderDivs $ PlotData <$> areq textareaField "Data" Nothing

makePlot :: String -> Textarea -> IO Bool
makePlot filename rawData =
    EP.plot (EP.PNG filename) $ rawToFloats rawData

makeFilename :: String -> String
makeFilename uuid = "static/img/plots/" <> uuid <> ".png"

rawToFloats :: Textarea -> [(Float,Float)]
rawToFloats =
    map ((read *** read) . to2Tuple . words) . lines . T.unpack . unTextarea

to2Tuple :: [a] -> (a,a)
to2Tuple [x,y]  = (x,y)
to2Tuple _      = error "Only list of length 2"
