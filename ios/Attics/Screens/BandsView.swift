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
    var storage: AppStorageManager
    
    private let apiClient: APIClient
    private var request: AnyCancellable?
    
    init(apiClient: APIClient, storage: AppStorageManager, onBandClick: @escaping (BandWithMetadata) -> ()) {
        self.apiClient = apiClient
        self.storage = storage
        self.onBandClick = onBandClick
    }
    
    func load() {
        let stored = storage.bands
        if !stored.isEmpty {
            bands = .success(stored.sorted(by: { $0.name < $1.name }))
        }
        request = apiClient.getBands()
            .receive(on: DispatchQueue.main)
            .sink(receiveCompletion: { completion in
                switch completion {
                case .failure(let error):
                    switch self.bands {
                    case .success(_):
                        break
                    default:
                        self.bands = .error(error)
                    }
                default:
                    break
                }
            }, receiveValue: { bands in
                self.bands = .success(bands.sorted(by: { $0.name < $1.name }))
                self.storage.bands = bands
                
                self.objectWillChange.send()
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
        guard asyncFileLoad({ image in
            self.state = .loaded(image)
        }, failure: loadFromUrl) else {
            loadFromUrl()
            return
        }
    }
    
    private func loadFromUrl() {
        print("loading from url")
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
    
    func asyncFileLoad(_ completion: @escaping (UIImage) -> (), failure: @escaping () -> ()) -> Bool {
        let folder = try! Folder.applicationSupport.createSubfolderIfNeeded(withName: "Images")
        guard let file = try? folder.file(named: name + ".jpeg") else { return false }
        DispatchQueue.init(label: "background", qos: .userInteractive).async {
            if let data = try? file.read(),
               let image = UIImage(data: data) {
                DispatchQueue.main.async {
                    completion(image)
                }
            } else {
                failure()
            }
        }
        return true
    }
    
    func loadFromFile() throws -> UIImage? {
        let folder = try Folder.applicationSupport.createSubfolderIfNeeded(withName: "Images")
        let file = try folder.file(named: name + ".jpeg")
        return UIImage(data: try file.read())
    }
    
    func writeFile(image: UIImage) throws {
        let folder = try Folder.applicationSupport.createSubfolderIfNeeded(withName: "Images")
        let file = try folder.createFileIfNeeded(at: name + ".jpeg")
        
        try file.write(image.jpegData(compressionQuality: 0.10)!)
    }
    
    var body: some View {
        _body().onAppear(perform: load)
    }
    
    @ViewBuilder
    func _body() -> some View {
        switch state {
        case .loading: LoadingComponent(retry: nil)
        case .loaded(let image): Image(uiImage: image).resizable().scaledToFill()
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
                RemoteImage(name: URL(string: band.logoUrl)!.lastPathComponent, url: URL(string: band.logoUrl)!)
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
