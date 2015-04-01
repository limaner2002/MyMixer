#include <AudioToolbox/AudioQueue.h>
#include <AudioToolbox/AudioFile.h>
#include <string>

#ifdef ENABLE_DEBUG
#include <iostream>
#include <cstdio>
#define DEBUG(message,...) fprintf(stderr, "%s::%d:\t" message, __FILE__,__LINE__,##__VA_ARGS__); \
  fflush(stderr);
#else
#define DEBUG(message,...) ((void) 0)
#endif

// Must find out how to use the system's version.
#define fsRdPerm 0x01
static const int kNumberBuffers = 3;                              // 1

void checkStatus(OSStatus status, std::string msg);

struct AQPlayerState {
  AudioStreamBasicDescription   mDataFormat;                    // 2
  AudioQueueRef                 mQueue;                         // 3
  AudioQueueBufferRef           mBuffers[kNumberBuffers];       // 4
  AudioFileID                   mAudioFile;                     // 5
  UInt32                        bufferByteSize;                 // 6
  SInt64                        mCurrentPacket;                 // 7
  UInt32                        mNumPacketsToRead;              // 8
  AudioStreamPacketDescription  *mPacketDescs;                  // 9
  bool                          mIsRunning;                     // 10
};

class Player{
public:
  AQPlayerState state;
private:
  CFURLRef getCFURL(const char *path);
  void getDataFormat();
  OSStatus status;
public:
  Player();
  void openFile(const char *path);
  void createQueue(AudioQueueOutputCallback handleOutputBuffer);
  void setBufferSize(void (*deriveBufferSize)(
					      AudioStreamBasicDescription&,
					      UInt32,
					      Float64,
					      UInt32*,
					      UInt32*)
		     );
  void allocatePacketDescriptionsArray();
  void setMagicCookie();
  void primeAudioQueueBuffers(AudioQueueOutputCallback handleOutputBuffer);
  void setPlaybackGain(Float32 gain);
  void start();
  ~Player();
};
