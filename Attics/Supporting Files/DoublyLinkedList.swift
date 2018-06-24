//
//  DoublyLinkedList.swift
//  Attics
//
//  Created by Zachary Wood on 6/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

struct DoublyLinkedList<T> {
    
    private class Node {
        var data: T? = nil
        var previous: Node? = nil
        var next: Node? = nil
        
        init(containing data: T? = nil) {
            self.data = data
        }
    }
    
    private var head = Node()
    private var tail = Node()
    private var currentIndex = 0
    
    
    var firstElement: T? {
        return head.next?.data
    }
    
    var lastElement: T? {
        return tail.previous?.data
    }
    
    var currentElement: T? {
        return nil
    }
    
    
    mutating func append(_ data: T) {
        if (head.next == nil) {
            let newNode = Node(containing: data)
            newNode.previous = head
            newNode.next = tail
            
            head.next = newNode
            tail.previous = newNode
        }
    }
}
