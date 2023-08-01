#' Create UI to Record User Audio
#'
#' @description The user-interface side of the Shiny module to record user audio in Speech Production experiments. A participant chooses when to begin recording and when to stop by clicking the buttons created in this module. Requires the server-side function \code{\link{recordServer}}.
#' @param id The input ID associated with the record module. Must be the same as the id of `recordServer()`.
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
#'     # which is saved in the apps input list as "(id)-stop"
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
#'
recordUI <- function(id = "record",
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

  txt <- "shinyjs.webAudioRecorder = function(params) {
  var defaultParams =
  {
    recId : '',
    startId : '',
    stopId : ''
  };

  params = shinyjs.getParams(params, defaultParams);

        var startRecordingButton = document.getElementById(params.startId);
        var stopRecordingButton = document.getElementById(params.stopId);


        var leftchannel = [];
        var rightchannel = [];
        var recorder = null;
        var recordingLength = 0;
        var volume = null;
        var mediaStream = null;
        var context = null;
        var blob = null;
        var ready = params.recId+'-ready'
        var output = params.recId+'-audio'

        startRecordingButton.addEventListener('click', function () {
            // Initialize recorder
            navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || navigator.msGetUserMedia;
            navigator.getUserMedia(
            {
                audio: true
            },
            function (e) {
                console.log('user consent');
                Shiny.setInputValue(ready, 'yes');

                // creates the audio context
                window.AudioContext = window.AudioContext || window.webkitAudioContext;
                context = new AudioContext();

                // creates an audio node from the microphone incoming stream
                mediaStream = context.createMediaStreamSource(e);

                // https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createScriptProcessor
                // bufferSize: the onaudioprocess event is called when the buffer is full
                var bufferSize = 4096;
                var numberOfInputChannels = 2;
                var numberOfOutputChannels = 2;
                if (context.createScriptProcessor) {
                    recorder = context.createScriptProcessor(bufferSize, numberOfInputChannels, numberOfOutputChannels);
                } else {
                    recorder = context.createJavaScriptNode(bufferSize, numberOfInputChannels, numberOfOutputChannels);
                }

                recorder.onaudioprocess = function (e) {
                    leftchannel.push(new Float32Array(e.inputBuffer.getChannelData(0)));
                    rightchannel.push(new Float32Array(e.inputBuffer.getChannelData(1)));
                    recordingLength += bufferSize;
                }

                // we connect the recorder
                mediaStream.connect(recorder);
                recorder.connect(context.destination);
            },
                        function (e) {
                            console.error(e);
                        });
        });

        stopRecordingButton.addEventListener('click', function () {

            // stop recording
            recorder.disconnect(context.destination);
            mediaStream.disconnect(recorder);

            // we flat the left and right channels down
            // Float32Array[] => Float32Array
            var leftBuffer = flattenArray(leftchannel, recordingLength);
            var rightBuffer = flattenArray(rightchannel, recordingLength);
            // we interleave both channels together
            // [left[0],right[0],left[1],right[1],...]
            //var interleaved = interleave(leftBuffer, rightBuffer);

            // we create our wav file
            var buffer = new ArrayBuffer(44 + leftBuffer.length * 2);
            var view = new DataView(buffer);

            // RIFF chunk descriptor
            writeUTFBytes(view, 0, 'RIFF');
            view.setUint32(4, 36 + leftBuffer.length * 2, true);
            writeUTFBytes(view, 8, 'WAVE');
            // FMT sub-chunk
            writeUTFBytes(view, 12, 'fmt ');
            view.setUint32(16, 16, true); // chunkSize
            view.setUint16(20, 1, true); // wFormatTag
            view.setUint16(22, 1, true); // wChannels: stereo (2 channels), change to 1 for mono
            view.setUint32(24, context.sampleRate, true); // dwSamplesPerSec
            view.setUint32(28, context.sampleRate * 2, true); // dwAvgBytesPerSec, change to *2 for mono
            view.setUint16(32, 4, true); // wBlockAlign
            view.setUint16(34, 16, true); // wBitsPerSample
            // data sub-chunk
            writeUTFBytes(view, 36, 'data');
            view.setUint32(40, leftBuffer.length * 2, true);

            // write the PCM samples
            var index = 44;
            var volume = 1;
            for (var i = 0; i < leftBuffer.length; i++) {
                view.setInt16(index, leftBuffer[i] * (0x7FFF * volume), true);
                index += 2;
            }

            // our final blob
            blob = new Blob([view], { type: 'audio/wav' });
            var reader = new FileReader();
            reader.readAsDataURL(blob);
            reader.onloadend = function(){
                Shiny.setInputValue(output, reader.result);
            }
        });

        function flattenArray(channelBuffer, recordingLength) {
            var result = new Float32Array(recordingLength);
            var offset = 0;
            for (var i = 0; i < channelBuffer.length; i++) {
                var buffer = channelBuffer[i];
                result.set(buffer, offset);
                offset += buffer.length;
            }
            return result;
        }

        function writeUTFBytes(view, offset, string) {
            for (var i = 0; i < string.length; i++) {
                view.setUint8(offset + i, string.charCodeAt(i));
            }
        }
        }"

  #shiny::addResourcePath("WAR", system.file("WAR", package = "speechcollectr"))

  ui <- shiny::tags$span(
    shiny::tags$head(
      #shiny::includeScript(file.path("WAR", "WebAudioRecorder.min.js")),
                     shinyjs::useShinyjs(),
                     shinyjs::extendShinyjs(text = txt,
                                            functions = "webAudioRecorder")),
    shinyjs::hidden(shiny::tags$div(id = paste0(id), style = paste0("text-align:", align,";"),
                                    shinyjs::hidden(shiny::tags$div(id = paste0(id, "-stimDiv"),
                                                                    style = paste0("text-align:", align,";"),
                                                                    shiny::tags$h3(shiny::textOutput(paste0(id, "-stim"))))),

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
