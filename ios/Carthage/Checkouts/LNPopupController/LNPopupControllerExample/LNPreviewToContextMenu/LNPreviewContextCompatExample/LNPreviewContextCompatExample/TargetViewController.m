//
//  TargetViewController.m
//  LNPreviewContextCompatExample
//
//  Created by Leo Natan (Wix) on 9/20/19.
//  Copyright © 2019 LeoNatan. All rights reserved.
//

#import "TargetViewController.h"
#import <LNPreviewToContextMenu/UIPreviewAction+Images.h>

@interface TargetViewController ()

@end

@implementation TargetViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view.
}

- (NSArray<id<UIPreviewActionItem>> *)previewActionItems
{
	UIPreviewAction* first = [UIPreviewAction actionWithTitle:@"Like" style:UIPreviewActionStyleDefault handler:^(UIPreviewAction * _Nonnull action, UIViewController * _Nonnull previewViewController) {
		NSLog(@"👍 Liked");
	}];
	if (@available(iOS 13.0, *)) {
		first.image = [UIImage systemImageNamed:@"hand.thumbsup.fill"];
	}
	
	UIPreviewAction* favorite = [UIPreviewAction actionWithTitle:@"Favorite" style:UIPreviewActionStyleDefault handler:^(UIPreviewAction * _Nonnull action, UIViewController * _Nonnull previewViewController) {
		NSLog(@"⭐️ Favorited");
	}];
	if (@available(iOS 13.0, *)) {
		favorite.image = [UIImage systemImageNamed:@"heart"];
	}
	
	UIPreviewAction* repost = [UIPreviewAction actionWithTitle:@"Repost" style:UIPreviewActionStyleDefault handler:^(UIPreviewAction * _Nonnull action, UIViewController * _Nonnull previewViewController) {
		NSLog(@"🔄 Reposted");
	}];
	if (@available(iOS 13.0, *)) {
		repost.image = [UIImage systemImageNamed:@"bubble.left"];
	}
	
	UIPreviewAction* repost2 = [UIPreviewAction actionWithTitle:@"Repsot With Comment" style:UIPreviewActionStyleDefault handler:^(UIPreviewAction * _Nonnull action, UIViewController * _Nonnull previewViewController) {
		NSLog(@"🔄💬 Reposted with comment");
	}];
	if (@available(iOS 13.0, *)) {
		repost2.image = [UIImage systemImageNamed:@"quote.bubble"];
	}
	
	UIPreviewActionGroup* menu = [UIPreviewActionGroup actionGroupWithTitle:@"More..." style:UIPreviewActionStyleDefault actions:@[
		favorite,
		repost,
		repost2
	]];
	if (@available(iOS 13.0, *)) {
		menu.image = [UIImage systemImageNamed:@"ellipsis.circle"];
	}
	
	UIPreviewAction* delete = [UIPreviewAction actionWithTitle:@"Delete" style:UIPreviewActionStyleDestructive handler:^(UIPreviewAction * _Nonnull action, UIViewController * _Nonnull previewViewController) {
		NSLog(@"🗑 Deleted");
	}];
	if (@available(iOS 13.0, *)) {
		delete.image = [UIImage systemImageNamed:@"trash"];
	}
	
	return @[first, menu, delete];
}

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/

@end
