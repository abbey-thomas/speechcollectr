URL = window.URL || window.webkitURL;

var gumStream; 						//stream from getUserMedia()
var rec; 							//Recorder.js object
var input; 							//MediaStreamAudioSourceNode we'll be recording

// shim for AudioContext when it's not avb.
var AudioContext = window.AudioContext || window.webkitAudioContext;
var audioContext; //audio context to help us record

Shiny.addCustomMessageHandler('startRec', startRecording);
Shiny.addCustomMessageHandler('stopRec', stopRecording);

function startRecording(el) {

  var constraints = { audio: true, video:false };

	/*
    	We're using the standard promise based getUserMedia()
    	https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia
	*/

	navigator.mediaDevices.getUserMedia(constraints).then(function(stream) {
		console.log("getUserMedia() success, stream created, initializing Recorder.js ...");

		/*
			create an audio context after getUserMedia is called
			sampleRate might change after getUserMedia is called, like it does on macOS when recording through AirPods
			the sampleRate defaults to the one set in your OS for your playback device

		*/
		audioContext = new AudioContext();

		/*  assign to gumStream for later use  */
		gumStream = stream;

		/* use the stream */
		input = audioContext.createMediaStreamSource(stream);
		Shiny.setInputValue("rec-ready", "yes", {priority: "event"});
		rec = new Recorder(input,{numChannels:1});

		rec.record();

		console.log("Recording started");

	}).catch(function(err) {
	  		Shiny.setInputValue("ready", null);

	});
}

function stopRecording(el2) {
	console.log("stopButton clicked");
	rec.stop();

	gumStream.getAudioTracks()[0].stop();

	//create the wav  and pass it on to createDownloadLink
	rec.exportWAV(function createDownloadLink(blob) {
	var reader = new FileReader();
            reader.readAsDataURL(blob);
            reader.onloadend = function(){
              Shiny.setInputValue(el2,reader.result);
            };
});
}






