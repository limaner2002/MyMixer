#include "Player.hpp"

/* This needs to be replaced to make use of AudioFileReadPacketData */
static void HandleOutputBuffer (
				void                *aqData,
				AudioQueueRef       inAQ,
				    AudioQueueBufferRef inBuffer
				) {
  struct AQPlayerState *pAqData = (struct AQPlayerState *) aqData;        // 1
  if (pAqData->mIsRunning == false) return;                     // 2
  UInt32 numBytesReadFromFile;                              // 3
  UInt32 numPackets = pAqData->mNumPacketsToRead;           // 4
  OSStatus status = AudioFileReadPackets (
			pAqData->mAudioFile,
			false,
			&numBytesReadFromFile,
			pAqData->mPacketDescs,
			pAqData->mCurrentPacket,
			&numPackets,
			inBuffer->mAudioData
			);
  checkStatus(status, "There was an error reading the packet data.");
  if (numPackets > 0) {                                     // 5
    inBuffer->mAudioDataByteSize = numBytesReadFromFile;  // 6
    status = AudioQueueEnqueueBuffer (
			     pAqData->mQueue,
			     inBuffer,
			     (pAqData->mPacketDescs ? numPackets : 0),
			                 pAqData->mPacketDescs
			     );
    checkStatus(status, "There was an error enqueuing the buffer.");
    pAqData->mCurrentPacket += numPackets;                // 7
  } else {
    status = AudioQueueStop (
		    pAqData->mQueue,
		                false
		    );
    checkStatus(status, "There was an error stopping the queue.");
    pAqData->mIsRunning = false;
  }
}

void DeriveBufferSize (
		       AudioStreamBasicDescription &ASBDesc,                            // 1
		       UInt32                      maxPacketSize,                       // 2
		       Float64                     seconds,                             // 3
		       UInt32                      *outBufferSize,                      // 4
		       UInt32                      *outNumPacketsToRead                 // 5
		       ) {
  static const int maxBufferSize = 0x50000;                        // 6
  static const int minBufferSize = 0x4000;                         // 7

  if (ASBDesc.mFramesPerPacket != 0) {                             // 8
            Float64 numPacketsForTime =
	      ASBDesc.mSampleRate / ASBDesc.mFramesPerPacket * seconds;
	    *outBufferSize = numPacketsForTime * maxPacketSize;
  } else {                                                         // 9
            *outBufferSize =
	                  maxBufferSize > maxPacketSize ?
	      maxBufferSize : maxPacketSize;
  }

  if (                                                             // 10
              *outBufferSize > maxBufferSize &&
	              *outBufferSize > maxPacketSize
								   )
    *outBufferSize = maxBufferSize;
  else {                                                           // 11
    if (*outBufferSize < minBufferSize)
      *outBufferSize = minBufferSize;
  }

  *outNumPacketsToRead = *outBufferSize / maxPacketSize;           // 12
}

extern "C" {
  int loadLocalFile(const char *path){
    Player p;
    AudioQueueOutputCallback handleOutputBuffer = &HandleOutputBuffer;
    void (*deriveBufferSize)(AudioStreamBasicDescription&,
			     UInt32,
			     Float64,
			     UInt32*,
			     UInt32*);

    deriveBufferSize = &DeriveBufferSize;

    p.openFile(path);
    p.createQueue(handleOutputBuffer);
    p.setBufferSize(deriveBufferSize);
    p.allocatePacketDescriptionsArray();
    p.setMagicCookie();
    p.primeAudioQueueBuffers(handleOutputBuffer);
    p.setPlaybackGain(1.0);
    p.start();

    return 0;
  }
}
