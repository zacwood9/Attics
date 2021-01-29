//
//  MigrationErrorView.swift
//  Attics
//
//  Created by Zachary Wood on 1/26/21.
//  Copyright © 2021 Zachary Wood. All rights reserved.
//

import SwiftUI

struct MigrationErrorView: View {
    var body: some View {
        VStack {
            Image(systemName: "wifi.exclamationmark").font(.system(size: 48))
            Text("An error occured while migrating your data.").bold()
            Divider()
                .padding(.bottom)
            Text("Attics v1.5 requires an internet connection on first launch to migrate your downloads to a new internal format. Please force quit the app, make sure your internet is working, and try again.")
            Text("It's also possible Attics is down right now. If your internet is working and you're still seeing this error, please wait and try again, or send me an email at zac.wood@hey.com.")
        }
    }
}

struct MigrationErrorView_Previews: PreviewProvider {
    static var previews: some View {
        MigrationErrorView()
    }
}
