import SwiftUI
import Combine
import Files
import AtticsCore

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
        cancellable = URLSession.shared.dataTaskPublisher(for: url)
            .map(\.data)
            .map(UIImage.init)
            .sink(receiveCompletion: { _ in }, receiveValue: { image in
                guard let image else {
                    logger.error("Failed to initialize UIImage for \(name).")
                    return
                }
                
                do {
                    try writeFile(image: image)
                    self.state = .loaded(image)
                } catch {
                    logger.error("Failed to write image \(name): \(error)")
                }
            })
    }
    
    func asyncFileLoad(_ completion: @escaping (UIImage) -> (), failure: @escaping () -> ()) -> Bool {
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
        let file = try folder.file(named: name + ".jpeg")
        return UIImage(data: try file.read())
    }
    
    func writeFile(image: UIImage) throws {
        let file = try folder.createFileIfNeeded(at: name + ".jpeg")
        try file.write(image.jpegData(compressionQuality: 0.10)!)
    }
    
    var body: some View {
        _body().onAppear(perform: load)
    }
    
    @ViewBuilder
    func _body() -> some View {
        switch state {
        case .loading: ProgressView()
        case .loaded(let image): Image(uiImage: image).resizable().scaledToFill()
        }
    }
    
    var folder: Folder {
        app.persistence.images
    }
}
