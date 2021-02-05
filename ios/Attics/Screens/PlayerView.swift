//
//  PlayerView.swift
//  Attics
//
//  Created by Zachary Wood on 11/6/20.
//

import SwiftUI

struct PlayerView: View {
    @ObservedObject var viewModel: RecordingViewModel
    @ObservedObject var player: MusicPlayer
    @State private var time: Double = 0
    
    var body: some View {
        if let state = player.state {
            view(state)
        } else {
            LoadingComponent(retry: nil)
        }
    }
    
    
    func view(_ state: MusicPlayer.State) -> some View {
        return VStack {
            ScrollView {
                PlayerHeader(viewModel: viewModel)
                SongList(
                    viewModel: viewModel,
                    musicPlayer: player,
                    songClick: { player.play($0, state.playlist) }
                ).environmentObject(player)
            }
            Slider(value: $player.percentFinished, in: 0...1, onEditingChanged: player.seekStateDidChange)
                .padding()
            
            HStack {
                Text(player.currentTime)
                Spacer()
                Text(player.duration)
            }.padding()
            
            HStack(alignment: .center, spacing: 40) {
                Image(systemName: "backward.fill")
                    .onTapGesture {
                        player.previousTrack()
                    }
                Image(systemName: state.playing ? "pause.fill" : "play.fill")
                    .onTapGesture {
                        state.playing ? player.pause() : player.resume()
                    }
                Image(systemName: "forward.fill")
                    .onTapGesture {
                        player.nextTrack()
                    }
            }
            .font(.system(size: 40))
            .foregroundColor(Color(#colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)))
        }
    }
}

struct MediaControls : View {
    @ObservedObject var player: MusicPlayer
    
    var body: some View {
        switch player.state {
        case .some(let state):
            VStack {
                Slider(value: $player.percentFinished, in: 0...1, onEditingChanged: player.seekStateDidChange)
                    .padding([.leading, .trailing, .top])
                
                HStack {
                    Text(player.currentTime)
                    Spacer()
                    Text(player.duration)
                }.padding([.leading, .trailing, .bottom])
                
                HStack(alignment: .center, spacing: 40) {
                    Image(systemName: "backward.fill")
                        .onTapGesture {
                            player.previousTrack()
                        }
                    Image(systemName: state.playing ? "pause.fill" : "play.fill")
                        .onTapGesture {
                            state.playing ? player.pause() : player.resume()
                        }
                    Image(systemName: "forward.fill")
                        .onTapGesture {
                            player.nextTrackForce()
                        }
                }
                .padding(.bottom)
                .font(.system(size: 40))
                .foregroundColor(Color(#colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)))
            }
        case .none: Text("waiting")
        }
        
    }
}
