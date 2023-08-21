URL = window.URL || window.webkitURL;

var gumStream; 						//stream from getUserMedia()
var rec; 							//Recorder.js object
var input; 							//MediaStreamAudioSourceNode we'll be recording

// shim for AudioContext when it's not avb.
var AudioContext = window.AudioContext || window.webkitAudioContext;
var audioContext; //audio context to help us record

var count = 0;
var filename = "init.wav";

let startBtns = document.querySelectorAll('.startRec');
let stopBtns = document.querySelectorAll('.stopRec');

startBtns.forEach(function (i) {
  i.addEventListener('click', startRecording);
});

stopBtns.forEach(function (i) {
  i.addEventListener('click', stopRecording);
});

function assignFilename(r_file) {
  filename = r_file;
}

//var recordButton = document.getElementById("recordButton");
//var stopButton = document.getElementById("stopButton");
//var pauseButton = document.getElementById("pauseButton");

//add events to those 2 buttons
//recordButton.addEventListener("click", startRecording);
//stopButton.addEventListener("click", stopRecording);
//pauseButton.addEventListener("click", pauseRecording);

function startRecording() {
	console.log("recordButton clicked");
	count++

	/*
		Simple constraints object, for more advanced audio features see
		https://addpipe.com/blog/audio-constraints-getusermedia/
	*/

    var constraints = { audio: true, video:false }

 	/*
    	Disable the record button until we get a success or fail from getUserMedia()
	*/

	//recordButton.disabled = true;
	//stopButton.disabled = false;
	//pauseButton.disabled = false

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
		Shiny.setInputValue("js_count", count);
		Shiny.setInputValue("ready"+count, "yes");

		/*
			Create the Recorder object and configure to record mono sound (1 channel)
			Recording 2 channels  will double the file size
		*/
		rec = new Recorder(input,{numChannels:1})

		//start the recording process
		rec.record()

		console.log("Recording started");

	}).catch(function(err) {
	  	//enable the record button if getUserMedia() fails
    	//recordButton.disabled = false;
    	//stopButton.disabled = true;
    	//pauseButton.disabled = true
	});
}

//function pauseRecording(){
	//console.log("pauseButton clicked rec.recording=",rec.recording );
	//if (rec.recording){
		//pause
		//rec.stop();
		//pauseButton.innerHTML="Resume";
	//}else{
		//resume
		//rec.record()
		//pauseButton.innerHTML="Pause";

	//}
//}

function stopRecording() {
	console.log("stopButton clicked");

	Shiny.addCustomMessageHandler("assign_fn", assignFilename);
	rec.stop();

	//stop microphone access
	gumStream.getAudioTracks()[0].stop();

	//create the wav blob and pass it on to createDownloadLink
	rec.exportWAV(createDownloadLink);
}

function createDownloadLink(blob) {

	var url = URL.createObjectURL(blob);
	var reader = new FileReader();
            reader.readAsDataURL(blob);
            reader.onloadend = function(){
                Shiny.setInputValue('audioOut'+count, reader.result);
            };

  var xhr=new XMLHttpRequest();
		  xhr.onload=function(e) {
		      if(this.readyState === 4) {
		          console.log("Server returned: ",e.target.responseText);
		      }
		  };
		  var fd=new FormData();
		  fd.append("audio_data",blob, filename);
		  xhr.open("POST","upload.php",true);
		  xhr.send(fd);
}
