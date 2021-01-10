//
//  AppState.swift
//  Attics
//
//  Created by Zachary Wood on 12/2/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

struct AppState {
    let id: String
    
    var year: UserDefaultStore<Year>
    var show: UserDefaultStore<Show>
    var source: UserDefaultStore<Source>
    var songIndex: UserDefaultStore<Int>
    var songTime: UserDefaultStore<Double>
    
    init(id: String) {
        self.id = id
        
        self.year = UserDefaultStore(withKey: "\(id)-year")
        self.show = UserDefaultStore(withKey: "\(id)-show")
        self.source = UserDefaultStore(withKey: "\(id)-source")
        self.songIndex = UserDefaultStore(withKey: "\(id)-songIndex")
        self.songTime = UserDefaultStore(withKey: "\(id)-songTime")
    }
}
