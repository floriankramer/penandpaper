
import eventBus from '../eventbus'

export default class AudioServer {
  cache: Map<string, HTMLAudioElement> = new Map()
  muted: boolean = false

  streams: HTMLAudioElement[] = []

  constructor () {
    console.log('registering audio events.')
    eventBus.$on('/audio/play', (src: string, volume: number = 1) => { this.play(src, volume) })
    eventBus.$on('/audio/stream', (src: string, volume: number = 1) => { this.stream(src, volume) })
    eventBus.$on('/audio/stop-streams', () => { this.stopAllStreams() })
    eventBus.$on('/audio/mute', (m: boolean) => { this.setMuted(m) })
  }

  // Play a short sound that should be cached
  play (src: string, volume: number = 1) {
    if (this.muted) {
      return
    }
    let audio: HTMLAudioElement | undefined = this.cache.get(src)
    if (audio !== undefined) {
      if (!audio.paused) {
        audio = new Audio(src)
      }
    } else {
      audio = new Audio(src)
      this.cache.set(src, audio)
    }
    audio.volume = volume
    audio.play()
  }

  // Play a long piece of music that should not be cached explicitly
  stream (src: string, volume: number = 1) {
    let audio = new Audio(src)
    this.streams.push(audio)
    audio.loop = true
    audio.addEventListener('error', () => {
      this.stopStream(audio)
    })
    audio.volume = volume
    audio.play()
  }

  stopStream (audio: HTMLAudioElement) {
    audio.loop = false
    audio.pause()
    let idx = this.streams.findIndex((o) => o === audio)
    if (idx > -1) {
      this.streams.splice(idx)
    }
  }

  stopAllStreams () {
    this.streams.forEach((audio: HTMLAudioElement) => {
      audio.pause()
    })
    this.streams.splice(0, this.streams.length)
  }

  setMuted (muted: boolean) {
    this.muted = muted
  }
}
