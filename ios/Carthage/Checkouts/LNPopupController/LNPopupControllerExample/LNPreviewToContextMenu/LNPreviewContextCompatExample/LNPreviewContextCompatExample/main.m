//
//  main.m
//  LNPreviewContextCompatExample
//
//  Created by Leo Natan (Wix) on 9/20/19.
//  Copyright © 2019 LeoNatan. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "AppDelegate.h"

int main(int argc, char * argv[]) {
	NSString * appDelegateClassName;
	@autoreleasepool {
	    // Setup code that might create autoreleased objects goes here.
	    appDelegateClassName = NSStringFromClass([AppDelegate class]);
	}
	return UIApplicationMain(argc, argv, nil, appDelegateClassName);
}
