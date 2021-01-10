//
//  BandsView.swift
//  Attics
//
//  Created by Zachary Wood on 1/5/21.
//  Copyright © 2021 Zachary Wood. All rights reserved.
//

import Combine
import SwiftUI

class BandsViewModel : ObservableObject {
    @Published var bands: APIResult<[BandWithMetadata]> = .loading
    var onBandClick: (BandWithMetadata) -> ()
    
    private let apiClient: APIClient
    private var request: AnyCancellable?
    
    init(apiClient: APIClient, onBandClick: @escaping (BandWithMetadata) -> ()) {
        self.apiClient = apiClient
        self.onBandClick = onBandClick
    }
    
    func load() {
        request = apiClient.getBands()
            .receive(on: DispatchQueue.main)
            .sink(receiveCompletion: { completion in
                switch completion {
                case .failure(let error):
                    self.bands = .error(error)
                default:
                    break
                }
            }, receiveValue: { bands in
                self.bands = .success(bands.sorted(by: { $0.name < $1.name }))
            })
    }
}

struct RemoteImage: View {
    enum LoadingState {
        case loading
        case loaded(UIImage)
    }
    
    let name: String
    let url: URL
    @State var state: LoadingState = .loading
    @State var cancellable: AnyCancellable?
    
    func load() {
        
        if let image = try? loadFromFile() {
            state = .loaded(image)
        } else {
            cancellable = URLSession.shared.dataTaskPublisher(for: url)
                .map(\.data)
                .map(UIImage.init)
                .sink(receiveCompletion: { _ in }, receiveValue: { image in
                    if let image = image {
                        if (try? writeFile(image: image)) == nil {
                            print("failed to write image")
                        }
                        self.state = .loaded(image)
                    } else {
                        print("wat")
                    }
                })
        }
    }
    
    func loadFromFile() throws -> UIImage? {
        let folder = try Folder.applicationSupport.createSubfolderIfNeeded(withName: "Images")
        let file = try folder.file(named: name + ".png")
        return UIImage(data: try file.read())
    }
    
    func writeFile(image: UIImage) throws {
        let folder = try Folder.applicationSupport.createSubfolderIfNeeded(withName: "Images")
        let file = try folder.createFileIfNeeded(at: name + ".png")
        try file.write(image.pngData()!)
    }
    
    var body: some View {
        _body().onAppear(perform: load)
    }
    
    @ViewBuilder
    func _body() -> some View {
        switch state {
        case .loading: LoadingComponent(retry: nil)
        case .loaded(let image): Image(uiImage: image).resizable().scaledToFit()
        }
    }
}

struct BandView: View {
    let band: BandWithMetadata
    let onClick: (BandWithMetadata) -> ()
    
    var body: some View {
        VStack(alignment: .center, spacing: 16) {
            Spacer()
            HStack {
                RemoteImage(name: band.collection, url: URL(string: band.logoUrl)!)
                    .frame(width: 80, height: 80, alignment: .center)
                    .cornerRadius(50)
                VStack {
                    HStack {
                        Text(band.name)
                            .font(.title)
                            .foregroundColor(.white)
                            .bold()
                        Spacer()
                    }
                    HStack {
                        Text("\(band.numPerformances) shows, \(band.numRecordings) recordings")
                            .font(.subheadline)
                            .foregroundColor(.white)
                        Spacer()
                    }
                }
            }
            Spacer()
        }
        .padding([.leading, .trailing], 8)
        .background(Color(#colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)))
        .cornerRadius(8)
        .contentShape(Rectangle())
        .onTapGesture { onClick(band) }
    }
}

//struct BandsView_Previews: PreviewProvider {
//    static var previews: some View {
////        BandsView()
//    }
//}
