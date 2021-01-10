//
//  ViewController.m
//  LNPreviewContextCompatExample
//
//  Created by Leo Natan (Wix) on 9/20/19.
//  Copyright © 2019 LeoNatan. All rights reserved.
//

#import "ViewController.h"

@interface ViewController () <UIViewControllerPreviewingDelegate>
{
	IBOutlet UIImageView* _previewImageView;
}

@end

@implementation ViewController

- (void)viewDidAppear:(BOOL)animated
{
	[super viewDidAppear:animated];
	
	id <UIViewControllerPreviewing> previewing = [self registerForPreviewingWithDelegate:self sourceView:_previewImageView];
}

- (nullable UIViewController *)previewingContext:(id <UIViewControllerPreviewing>)previewingContext viewControllerForLocation:(CGPoint)location
{
	previewingContext.sourceRect = CGRectInset(previewingContext.sourceView.bounds, 30, 30);
	
	return [self.storyboard instantiateViewControllerWithIdentifier:@"target"];
}

- (void)previewingContext:(id <UIViewControllerPreviewing>)previewingContext commitViewController:(UIViewController *)viewControllerToCommit
{
	[self presentViewController:viewControllerToCommit animated:YES completion:nil];
}

@end
