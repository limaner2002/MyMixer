#include "Player.hpp"

Player::Player(){
  this->state.mIsRunning = true;
}

void checkStatus(OSStatus status, std::string msg){
#ifdef ENABLE_DEBUG
  if(status != noErr){
    std::cout << msg << std::endl;
    exit(EXIT_FAILURE);
  }
#endif
}

CFURLRef Player::getCFURL(const char *path){
  return CFURLCreateFromFileSystemRepresentation(
						 NULL,
						 (const UInt8 *) path,
						 strlen(path),
						 false
						 );
}

void Player::getDataFormat(){
  UInt32 dataFormatSize = sizeof(this->state.mDataFormat);
  this->status = AudioFileGetProperty(
				this->state.mAudioFile,
				kAudioFilePropertyDataFormat,
				&dataFormatSize,
				&this->state.mDataFormat
				);

  checkStatus(this->status, "Could not get data format.");
}

void Player::createQueue(AudioQueueOutputCallback handleOutputBuffer){
  this->status = AudioQueueNewOutput(
				     &this->state.mDataFormat,
				     handleOutputBuffer,
				     &this->state,
				     CFRunLoopGetCurrent (),
				     kCFRunLoopCommonModes,
				     0,
				     &this->state.mQueue
				     );

  checkStatus(this->status, "Could not create new output queue.");
}

void Player::openFile(const char *path){
  CFURLRef audioFileURL = this->getCFURL(path);
  this->status = AudioFileOpenURL(
				  audioFileURL,
				  fsRdPerm,
				  0,
				  &this->state.mAudioFile
				  );
  checkStatus(this->status, "Could not open file");
  CFRelease(audioFileURL);
  this->getDataFormat();
}

void Player::setBufferSize(void (*deriveBufferSize)(
						    AudioStreamBasicDescription&,
						    UInt32,
						    Float64,
						    UInt32*,
						    UInt32*)
){
  UInt32 maxPacketSize;
  UInt32 propertySize = sizeof(maxPacketSize);
  this->status = AudioFileGetProperty(
		       this->state.mAudioFile,
		       kAudioFilePropertyPacketSizeUpperBound,
		       &propertySize,
		       &maxPacketSize
		       );
  checkStatus(this->status, "Could not set buffer size.");

  deriveBufferSize(
		   this->state.mDataFormat,
		   maxPacketSize,
		   0.5,
		   &this->state.bufferByteSize,
		   &this->state.mNumPacketsToRead
		   );
}

void Player::allocatePacketDescriptionsArray(){
  bool isFormatVBR = (
		      this->state.mDataFormat.mBytesPerPacket == 0 ||
		      this->state.mDataFormat.mFramesPerPacket == 0
		      );

  if (isFormatVBR) {
    this->state.mPacketDescs =
      (AudioStreamPacketDescription*) malloc (
					      this->state.mNumPacketsToRead * sizeof (AudioStreamPacketDescription)
					      );
    checkStatus(this->state.mPacketDescs == NULL, "Could not allocate packet descriptions.");
  } else {
    this->state.mPacketDescs = NULL;
  }
}

void Player::setMagicCookie(){
  UInt32 cookieSize = sizeof(UInt32);
  bool couldNotGetProperty =
    AudioFileGetPropertyInfo(
			     this->state.mAudioFile,
			     kAudioFilePropertyMagicCookieData,
			     &cookieSize,
			     NULL
			     );

  if (!couldNotGetProperty && cookieSize) {
    char* magicCookie =
      (char *) malloc (cookieSize);

    this->status = AudioFileGetProperty (
			  this->state.mAudioFile,
			  kAudioFilePropertyMagicCookieData,
			  &cookieSize,
			  magicCookie
			  );
    checkStatus(status, "Error geeting cookie size.");
    this->status = AudioQueueSetProperty (
			   this->state.mQueue,
			   kAudioQueueProperty_MagicCookie,
			   magicCookie,
			   cookieSize
			   );

    checkStatus(this->status, "Error creating the cookie.");
    free (magicCookie);
  }
}

void Player::primeAudioQueueBuffers(AudioQueueOutputCallback handleOutputBuffer){
  this->state.mCurrentPacket = 0;

  for (int i = 0; i < kNumberBuffers; ++i) {
    this->status = AudioQueueAllocateBuffer (
			      this->state.mQueue,
			      this->state.bufferByteSize,
			      &this->state.mBuffers[i]
			      );
    checkStatus(this->status, "Could not allocate buffer");
    handleOutputBuffer (
			&this->state,
			this->state.mQueue,
			this->state.mBuffers[i]
			);
  }
}

void Player::setPlaybackGain(Float32 gain){
  this->status = AudioQueueSetParameter(
			 this->state.mQueue,
			 kAudioQueueParam_Volume,
			 gain
			 );
  checkStatus(this->status, "Could not set playback gain.");
}

void Player::start(){
  this->status = AudioQueueStart(
		  this->state.mQueue,
		  NULL
		  );
  checkStatus(this->status, "Could not start audio queue.");
  do {
    CFRunLoopRunInMode(
		       kCFRunLoopDefaultMode,
		       0.25,
		       false
		       );
  } while(this->state.mIsRunning);

  CFRunLoopRunInMode(
		     kCFRunLoopDefaultMode,
		     1,
		     false
		     );
}

Player::~Player(){
  AudioQueueDispose(
		    this->state.mQueue,
		    true
		    );

  AudioFileClose(this->state.mAudioFile);

  free(this->state.mPacketDescs);
}

