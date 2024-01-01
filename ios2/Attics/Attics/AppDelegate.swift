//
//  AppDelegate.swift
//  Attics
//
//  Created by Zachary Wood on 12/16/23.
//

import UIKit

class AppDelegate: NSObject, UIApplicationDelegate {
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey : Any]? = nil) -> Bool {
        
        return true
    }
    
    func applicationDidEnterBackground(_ application: UIApplication) {
        app.beforeTermination()
    }
    
    func applicationWillTerminate(_ application: UIApplication) {
        app.beforeTermination()
    }
}
