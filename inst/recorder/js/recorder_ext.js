/* WebAudioRecorder taken from https://github.com/addpipe/simple-web-audio-recorder-demo */

  /* The MIT License (MIT)

Copyright (c) 2020 Abbey Thomas

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the 'Software'), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

  // These functions, with minor modifications, are taken from Yuji Miyane's (2015) app. On github at https://github.com/heeringa0/simple-web-audio-recorder
shinyjs.webAudioRecorder = function(params)
{
  var defaultParams =
  {
    ets : '',
    mod_id : ''
  };

  params = shinyjs.getParams(params, defaultParams);

  // webkitURL is deprecated but nevertheless
  URL = window.URL || window.webkitURL;

  // stream from getUserMedia()
  var gumStream;

  // WebAudioRecorder object
  var recorder;

  // MediaStreamAudioSourceNode  we'll be recording
  var input;

  // holds selected encoding for resulting audio (file)
  var encodingType;

  // when to encode
  var encodeAfterRecord = true;

  // shim for AudioContext when it's not avb.
  var AudioContext = window.AudioContext || window.webkitAudioContext;
  var audioContext; //new audio context to help us record

  var encodingTypeSelect = params.ets;
  var record = params.mod_id+'-start'
  var stop = params.mod_id+'-stop'
  var ready = params.mod_id+'-ready'
  var output = params.mod_id+'-audio'


  // add events to those 2 buttons
  document.getElementById(params.mod_id+'-start').addEventListener('click', startRecording);
  document.getElementById(params.mod_id+'-stop').addEventListener('click',  stopRecording);

  function startRecording()
  {

    // Simple constraints object, for more advanced features see
    // https://addpipe.com/blog/audio-constraints-getusermedia/

    var constraints = { audio: true, video:false }

    // We're using the standard promise based getUserMedia()
  // https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia

  navigator.mediaDevices.getUserMedia(constraints).then(function(stream)
  {
    // create an audio context after getUserMedia is called
    // sampleRate might change after getUserMedia is called, like it does on macOS when recording through AirPods
    // the sampleRate defaults to the one set in your OS for your playback device

    audioContext = new AudioContext();

    // assign to gumStream for later use
    gumStream = stream;

    // use the stream
    input = audioContext.createMediaStreamSource(stream);

    // get the encoding
    encodingType = encodingTypeSelect;


    recorder = new WebAudioRecorder(input,
                                    {
                                      workerDir: 'inst/recorder/WAR/',
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

    // start the recording process
    recorder.startRecording();

    // Create an input id 'ready' with the value of 'yes' so the R user can control things like like disabling the record button in R.
    Shiny.setInputValue(params.mod_id+'-ready', 'yes');
  }).catch(function(err)
  {
    document.getElementById('browser_error').style.display = 'block';
    document.getElementById('again').style.display = 'block';

  });



  }

function stopRecording()
{
  // stop microphone access
  gumStream.getAudioTracks()[0].stop();

  // tell the recorder to finish the recording (stop recording + encode the recorded audio)
  recorder.finishRecording();
}

/* https://gist.github.com/primaryobjects/d6cdf5d31242a629b0e4cda1bfc4bff9 */

  function saveRecording(blob)
{
  var reader = new FileReader();
  reader.readAsDataURL(blob);

  reader.onloadend = function()
  {
    Shiny.setInputValue(output, reader.result);
  }
  }

}
