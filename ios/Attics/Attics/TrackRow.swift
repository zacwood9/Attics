//
//  TrackRow.swift
//  Attics
//
//  Created by Zachary Wood on 12/23/23.
//

import SwiftUI
import AtticsCore
import Combine

struct TrackRow: View {
    var index: Int
    var title: String
    var length: String
    var downloading: Bool
    var playing: Bool
    var disabled: Bool
    
    var body: some View {
        HStack {
            Text("\(index). ").fontWeight(.light).font(.footnote)
            Text(title)
            Spacer()
            
            if playing {
                PlayingAnimation()
            } else {
                Text(length).fontWeight(.light).font(.footnote)
            }
        }
        .contentShape(Rectangle())
        .foregroundStyle(disabled ? .gray : .primary)
    }
}

struct PlayingAnimation: View {
 
    @State private var drawingHeight = true
 
    var animation: Animation {
        return .linear(duration: 0.5).repeatForever()
    }
 
    var body: some View {
        HStack(spacing: 2) {
            bar(low: 0.4)
                .animation(animation.speed(1.5), value: drawingHeight)
            bar(low: 0.3)
                .animation(animation.speed(1.2), value: drawingHeight)
            bar(low: 0.5)
                .animation(animation.speed(1.0), value: drawingHeight)
        }
        .frame(width: 18)
        .onAppear{
            drawingHeight.toggle()
        }
    }
 
    func bar(low: CGFloat = 0.0, high: CGFloat = 1.0) -> some View {        
        RoundedRectangle(cornerRadius: 3)
            .fill(.atticsBlue.gradient)
            .frame(height: (drawingHeight ? high : low) * 18)
            .frame(height: 18, alignment: .bottom)
    }
}

extension ShapeStyle where Self == Color {
    static var atticsBlue: Color {
        .atticsBlue
    }
}


#Preview {
    List {
        TrackRow(index: 1, title: "Stella Blue", length: "4:35", downloading: true, playing: true, disabled: false)
        TrackRow(index: 2, title: "Scarlet Begonias", length: "9:55", downloading: true, playing: false, disabled: true)
    }
}
