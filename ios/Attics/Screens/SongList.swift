////
////  SongList.swift
////  Attics
////
////  Created by Zachary Wood on 11/9/20.
////
//
//import SwiftUI
//
//struct SongList: View {
//    @EnvironmentObject var player: MusicPlayer
//    
//    var songClick: (Song) -> ()
//    
//    var songs: [Song]
//    
//    var body: some View {
//        list(songs)
//    }
//    
//    func list(_ songs: [Song]) -> some View {
//        let a = player.state?.song.id ?? ""
//        
//        return ForEach(songs) { song in
//            let endView = a == song.id
//                ? AnyView(Image(systemName: "music.note"))
//                : AnyView(Text("\(song.track)"))
//            
//            let title = a == song.id ?
//                Text(song.title)
//                .font(.body)
//                .foregroundColor(Color.init(.label))
//                .bold()
//                : Text(song.title)
//                .font(.body)
//                .foregroundColor(Color.init(.label))
//            HStack {
//                endView
//                    .font(.footnote)
//                    .foregroundColor(Color.init(.tertiaryLabel))
//                title
//                Spacer()
//                AnyView(Text(song.length))
//                    .font(.callout)
//                    .foregroundColor(Color.init(.secondaryLabel))
//            }
//            .padding(EdgeInsets(top: 8, leading: 12, bottom: 8, trailing: 12))
//            .onTapGesture(count: 1, perform: { songClick(song) })
//            Divider()
//        }
//    }
//}
//
////struct SongList_Previews: PreviewProvider {
////    static var previews: some View {
//////        SongList()
////    }
////}
