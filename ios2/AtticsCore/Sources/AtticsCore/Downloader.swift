//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/24/23.
//

import Foundation
import Combine

public enum DownloadState {
    case waiting
    case downloading(Double)
    case finished
}

public class Downloader: ObservableObject {
    let p: Persistence
    
    init(p: Persistence) {
        self.p = p
    }
}
