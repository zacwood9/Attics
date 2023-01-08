//
//  Migrations.swift
//  Attics
//
//  Created by Zachary Wood on 12/2/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//
//  When building Attics, things change. For example, moving from
//  CoreData to a FileSystem store. This file contains functions which migrate
//  from old versions to new versions to facilitate these changes.

import Foundation
import Combine

func runMigrations(success: @escaping () -> (), failure: @escaping () -> (), useGuard: Bool = true) {
}
