package aoc.year2018.day09;

import java.util.LinkedList;
import java.util.List;

class Node {
    int value;
    Node next;
    Node prev;

    /**
     * Creates a new node with the given value.
     *
     * @param value value of the new node
     */
    Node(int value) {
        this.value = value;
        next = this;
        prev = this;
    }

    private Node(int value, Node prev, Node next) {
        this.value = value;
        this.next = next;
        this.prev = prev;
    }

    /**
     * Inserts a new node with value after itself.
     *
     * @param value value of the new node to be added
     * @return the node which was inserted
     */
    Node insertAfter(int value) {
        Node left = this;
        Node right = this.next;
        Node middle = new Node(value, left, right);
        left.next = middle;
        right.prev = middle;
        return middle;
    }

    /**
     * Removes the node behind itself.
     *
     * @return the value of node which was removed
     */
    int removeBefore() {
        Node left = this.prev.prev;
        Node middle = this.prev;
        Node right = this;
        left.next = right;
        right.prev = left;
        return middle.value;
    }

    List<Integer> toList() {
        List<Integer> l = new LinkedList<>();
        Node curr = this;
        while (true) {
            l.add(curr.value);
            if (curr.next == this) {
                break;
            }
            curr = curr.next;
        }
        return l;
    }
}
