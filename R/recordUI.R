#' Create UI to Record User Audio
#'
#' @description The user-interface side of the Shiny module to record user audio in Speech Production experiments. A participant chooses when to begin recording and when to stop by clicking the buttons created in this module. Requires the server-side function \code{\link{recordServer}}.
#' @param id The input ID associated with the record module. Must be the same as the id of `recordServer()`.
#' @param writtenStim Boolean. Do you have a written stimulus that you want your participant to read?
#' @param startText Character. The text label for the "start recording" button. Defaults to "RECORD".
#' @param startTextCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to "white".
#' @param startFillCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to the "green" hex code from Paul Tol's colorblind-safe \emph{bright} qualitative color scheme.
#' @param stopText Character. The text label for the "stop recording" button. Defaults to "STOP".
#' @param stopTextCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to "black".
#' @param stopFillCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to Defaults to the "red" hex code from Paul Tol's colorblind-safe \emph{bright} qualitative color scheme.
#' @param startInline Boolean. Should the 'start recording' button be on the same line as the preceding UI element?
#' @param stopInline Boolean. Should the 'stop recording' button be placed on the same line as the 'start recording' button?
#' @param align One of 'left', 'center', or 'right'. Should the elements in this UI be left-, center-, or right-aligned?
#'
#' @return A user interface (initially hidden; see examples below or \code{\link{recordServer}} for details on how to trigger the appearance of this interface) containing buttons to control recording of participant audio.
#' @note The "start" and "stop" buttons from this module can be accessed in the server code with the "id" of the module as follows: `input[["id-start"]]` or `input[["id-stop"]]`
#' @export
#' @family Audio recording module
#' @seealso Must be used with \code{\link{recordServer}}. Paul Tol's colorblind-safe palettes (the source of the default button colors) can be found at \url{https://personal.sron.nl/~pault/#sec:qualitative}.
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyjs)
#'   ui <- fluidPage(
#'     actionButton("begin", "BEGIN"),
#'     recordUI(id ="rec")
#'   )
#'
#'   # Using the default "NULL" trigger argument, the record server function will save
#'   # a new file every time the participant clicks start,
#'   # with a new name generated each time a participant clicks start.
#'   server <- function(input, output, session) {
#'     recording <- recordServer(id ="rec",
#'                           outPrefix = paste0("www/rec_samp"))
#'
#'     # You'll need to make the recording interface visible though!
#'     # Which can be accomplished with shinyjs::showElement()
#'     observeEvent(input$begin, {
#'       showElement("rec")
#'     })
#'   }
#'  shinyApp(ui = ui, server = server)
#' }
#'
#' # Alternatively an explicit trigger argument can be set as shown below
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyjs)
#'
#'   ui <- fluidPage(
#'     actionButton("begin", "BEGIN"),
#'     recordUI(id ="rec")
#'   )
#'
#'   # Now a new filename will be used if and only if the participant clicks "begin"
#'   # Otherwise, the same file will be over-written
#'   # This is useful if we want to give a participant multiple attempts at recording...
#'   server <- function(input, output, session) {
#'     recording <- recordServer(id ="rec",
#'                           trigger = reactive(input$begin),
#'                           outPrefix = paste0("www/rec_samp"))
#'
#'     # But let's say we don't want them to have more than 3 attempts to record the file
#'     # We can do this by attaching an event to the "stop" button of the recorder,
#'     # which is saved in the apps input list as "{id of record*()}-stop"
#'     observeEvent(input[["rec-stop"]], {
#'       if (recording()$n == 3) {
#'         hide("rec")
#'       }
#'     })
#'   }
#' shinyApp(ui = ui, server = server)
#' }
#'
#' @importFrom grDevices col2rgb
#' @importFrom gplots col2hex
#'
recordUI <- function(id = "record",
                     writtenStim = FALSE,
                     startText = "RECORD",
                     startTextCol = "white",
                     startFillCol = "#228833",
                     stopText = "STOP",
                     stopTextCol = "black",
                     stopFillCol = "#EE6677",
                     startInline = FALSE,
                     stopInline = TRUE,
                     align = "center") {
  if (tryCatch(is.matrix(col2rgb(startFillCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: startFillCol argument requires a valid color name or hexadecimal code.")
  }

  if (tryCatch(is.matrix(col2rgb(startTextCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: startTextCol argument requires a valid color name or hexadecimal code.")
  }

  if (tryCatch(is.matrix(col2rgb(stopFillCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: stopFillCol argument requires a valid color name or hexadecimal code.")
  }

  if (tryCatch(is.matrix(col2rgb(stopTextCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: stopTextCol argument requires a valid color name or hexadecimal code.")
  }

  txt <- "shinyjs.webAudioRecorder = function(params)
{
  var defaultParams =
  {
    ets : '',
    recId : '',
    startId : '',
    stopId : ''
  };

  params = shinyjs.getParams(params, defaultParams);
  URL = window.URL || window.webkitURL;
  var gumStream;
  var recorder;
  var input;
  var encodingType;
  var encodeAfterRecord = true;
  var AudioContext = window.AudioContext || window.webkitAudioContext;
  var audioContext; //new audio context to help us record
  var encodingTypeSelect = params.ets;
  var ready = params.recId+'-ready'
  var output = params.recId+'-audio'

  document.getElementById(params.startId).addEventListener('click', startRecording);
  document.getElementById(params.stopId).addEventListener('click',  stopRecording);

  function startRecording()
  {
    var constraints = { audio: true, video:false }
  navigator.mediaDevices.getUserMedia(constraints).then(function(stream)
  {
    audioContext = new AudioContext();
    gumStream = stream;
    input = audioContext.createMediaStreamSource(stream);
    encodingType = encodingTypeSelect;
    recorder = new WebAudioRecorder(input,
                                    {
                                      workerDir: 'WAR/', // must end with slash
                                      encoding: encodingType,
                                      numChannels: 1

                                    });
    recorder.onComplete = function(recorder, blob)
    {
      saveRecording(blob);
    }

    recorder.setOptions(
      {
        timeLimit: 240,
        encodeAfterRecord: encodeAfterRecord,
        ogg: {quality: 0.5},
        mp3: {bitRate: 160}
      });
    recorder.startRecording();
    Shiny.setInputValue(ready, 'yes');
  }).catch(function(err)
  {
    document.getElementById('browser_error').style.display = 'block';
    document.getElementById('again').style.display = 'block';
  });
  }

function stopRecording()
{
  gumStream.getAudioTracks()[0].stop();
  recorder.finishRecording();
}
  function saveRecording(blob)
{
  var reader = new FileReader();
  reader.readAsDataURL(blob);
  reader.onloadend = function()
  {
    Shiny.setInputValue(output, reader.result);
  }
}

}"

  shiny::addResourcePath("WAR", system.file("WAR", package = "speechcollectr"))

  ui <- shiny::tags$span(
    shiny::tags$head(shiny::includeScript("WAR/WebAudioRecorder.min.js"),
                     shinyjs::useShinyjs(),
                     shinyjs::extendShinyjs(text = txt,
                                            functions = "webAudioRecorder")),
    shinyjs::hidden(shiny::tags$div(id = paste0(id), style = paste0("text-align:", align,";"),
                                    if (isTRUE(writtenStim)) {
                                      shinyjs::hidden(shiny::tags$div(id = paste0(id, "-stimDiv"),
                                                                      style = paste0("text-align:", align,";"),
                                                                      shiny::tags$h3(shiny::textOutput(paste0(id, "-stim")))))
                                    },

                                    shiny::actionButton(paste0(id, "-start"), label = startText,
                                                        style = paste0("color: ", gplots::col2hex(startTextCol),
                                                                       "; background-color: ",
                                                                       gplots::col2hex(startFillCol)),
                                                        inline = startInline),

                                    shinyjs::disabled(
                                      shiny::actionButton(paste0(id, "-stop"), label = stopText,
                                                          style = paste0("color: ", gplots::col2hex(stopTextCol),
                                                                         "; background-color: ",
                                                                         gplots::col2hex(stopFillCol)),
                                                          inline = stopInline)),
                                    # shiny::uiOutput(paste0(id, "-replay")),
                                    shinyjs::hidden(shiny::actionButton(paste0(id, "-file"),
                                                                        label = ""))))

  )

  return(ui)
}
