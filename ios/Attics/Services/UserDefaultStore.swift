//
//  UserDefaultStore.swift
//  Attics
//
//  Created by Zachary Wood on 8/21/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

import Foundation

struct UserDefaultStore<T: Codable> {
    let key: String
    let defaultValue: T?
    
    var item: T? {
        get {
            if let savedItem = UserDefaults.standard.object(forKey: key) as? Data {
                if let loadedItem = try? JSONDecoder().decode(T.self, from: savedItem) {
                    return loadedItem
                }
            }
            return defaultValue
        }
        set {
            if let encoded = try? JSONEncoder().encode(newValue) {
                UserDefaults.standard.set(encoded, forKey: key)
            }            
        }
    }
    
    init(withKey key: String, defaultValue: T? = nil) {
        self.key = key
        self.defaultValue = defaultValue
    }
}
