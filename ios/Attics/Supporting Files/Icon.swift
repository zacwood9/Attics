//
//  Icon.swift
//  Attics
//
//  Created by Zachary Wood on 12/22/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

import SwiftUI
import FontAwesome

struct Icon: View {
    let faIcon: FontAwesome
    let size: CGFloat
    let padding: EdgeInsets
    
    var body: some View {
        Text(String.fontAwesomeIcon(name: faIcon))
            .font(.init(UIFont.fontAwesome(ofSize: size, style: .solid)))
            .padding(padding)
    }
    
    init(_ faIcon: FontAwesome, size: CGFloat, padding: EdgeInsets) {
        self.faIcon = faIcon
        self.size = size
        self.padding = padding
    }
}
