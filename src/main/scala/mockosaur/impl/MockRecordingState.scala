package mockosaur.impl

import mockosaur.model.Mock

import scala.ref.WeakReference

private[mockosaur] object MockRecordingState {

  // thread local so test runners can run test cases concurrently
  private val recordingMockForThread = new ThreadLocal[WeakReference[Mock]]

  def getRecordingMock(): Option[Mock] = {
    Option(recordingMockForThread.get()).flatMap(_.get)
  }

  def isRecording(mock: Mock): Boolean = {
    getRecordingMock().contains(mock)
  }

  def startRecording(mock: Mock): Unit = {
    recordingMockForThread.set(WeakReference(mock))
  }

  def stopRecording(): Unit = {
    recordingMockForThread.remove()
  }
}
