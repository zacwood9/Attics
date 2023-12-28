import SwiftUI
import AtticsCore
import LNPopupUI

struct PopupView: View {
    @ObservedObject var playlist: Playlist
    @EnvironmentObject var audioSystem: AudioSystem
    let openRecording: (String) -> Void
    
    enum SeekState {
        case notSeeking
        case userEditing(Double)
        case seeking(Double)
    }
    
    @State var seekState = SeekState.notSeeking
    
    var sliderBinding: Binding<Double> {
        Binding {
            switch seekState {
            case .notSeeking:
                audioSystem.currentProgress
            case .userEditing(let progress):
                progress
            case .seeking(let progress):
                progress
            }
        } set: { newValue in
            seekState = .userEditing(newValue)
        }
    }
    
    var progressString: String {
        switch seekState {
        case .notSeeking:
            return audioSystem.secondsPlayed.asTimeString(style: .positional)
        case .userEditing(let percent):
            fallthrough
        case .seeking(let percent):
            return (audioSystem.currentDuration * percent).asTimeString(style: .positional)
        }
    }
    
    var tracks: [Playlist.Track] {
        playlist.tracks
    }
    
    var body: some View {
        VStack {
            List {
                Section {
                    ForEach(tracks.indices, id: \.self) { i in
                        let track = tracks[i]
                        
                        TrackRow(index: i + 1, title: track.title, length: track.length, downloading: false, playing: playlist.currentTrack.id == track.id)
                            .onTapGesture {
                                audioSystem.play(id: track.id)
                            }
                    }
                }
                
                Section {
                    HStack {
                        Text("Open recording")
                        Spacer()
                        Image(systemName: "chevron.right").font(.subheadline)
                    }
                    .contentShape(Rectangle())
                    .onTapGesture {
                        guard playlist.playlistType == .recording else { return }
                        openRecording(playlist.playlistId)
                    }
                }
            }
            
            VStack(spacing: 0) {
                Slider(value: sliderBinding, in: 0...1) { editing in
                    if editing {
                        seekState = .seeking(sliderBinding.wrappedValue)
                    } else {
                        seekState = .seeking(sliderBinding.wrappedValue)
                        audioSystem.seek(toPercentage: sliderBinding.wrappedValue) {
                            self.seekState = .notSeeking
                        }
                    }
                }
                HStack {
                    Text(progressString)
                        .font(.subheadline)
                        .fontWeight(.light)
                    Spacer()
                    Text(audioSystem.currentDuration.asTimeString(style: .positional))
                        .font(.subheadline)
                        .fontWeight(.light)
                }
                HStack(spacing: 24) {
                    Button {
                        audioSystem.previousTrack()
                    } label: {
                        Image(systemName: "backward.fill")
                    }.buttonStyle(BorderlessButtonStyle())
                    
                    Button {
                        if audioSystem.isPlaying {
                            audioSystem.pause()
                        } else {
                            audioSystem.resume()
                        }
                    } label: {
                        Image(systemName: audioSystem.isPlaying ? "pause.fill" : "play.fill")
                    }.buttonStyle(BorderlessButtonStyle())
                    
                    Button {
                        audioSystem.nextTrack()
                    } label: {
                        Image(systemName: "forward.fill")
                    }.buttonStyle(BorderlessButtonStyle())
                }
                .font(.system(size: 42))
                .foregroundColor(.atticsBlue)
            }
            .cornerRadius(8)
            .padding()
            .background(Color(UIColor.systemBackground))
        }
        .popupTitle(verbatim: playlist.currentTrack.title, subtitle: playlist.currentTrack.album)
        .popupBarStyle(LNPopupBar.Style.prominent)
        .popupHapticFeedbackEnabled(true)
        .popupBarItems({
            Button(action: { audioSystem.isPlaying ? audioSystem.pause() : audioSystem.resume() }) {
                Image(systemName: audioSystem.isPlaying ? "pause.fill" : "play.fill")
            }
            Button(action: { audioSystem.nextTrack() }) {
                Image(systemName: "forward.fill")
            }
        })
    }
}
