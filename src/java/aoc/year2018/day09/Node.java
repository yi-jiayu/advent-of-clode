package aoc.year2018.day09;

import java.util.LinkedList;
import java.util.List;

public class Node {
    public Node head;
    public int value;
    public Node next;
    public Node prev;

    /**
     * Creates a new node with the given value. Its next and previous node are
     * initialised to itself, making it a circular list of a single element.
     *
     * @param value value of the new node
     */
    public Node(int value) {
        this.head = this;
        this.value = value;
        next = this;
        prev = this;
    }

    private Node(Node head, int value, Node prev, Node next) {
        this.head = head;
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
    public Node insertAfter(int value) {
        Node left = this;
        Node right = this.next;
        var middle = new Node(this.head, value, left, right);
        left.next = middle;
        right.prev = middle;
        return middle;
    }

    /**
     * Removes the node behind itself.
     *
     * @return the value of node which was removed
     */
    public int removeBefore() {
        Node left = this.prev.prev;
        Node middle = this.prev;
        Node right = this;
        left.next = right;
        right.prev = left;
        return middle.value;
    }

    public List<Integer> toList() {
        List<Integer> l = new LinkedList<>();
        Node curr = this.head;
        while (true) {
            l.add(curr.value);
            if (curr.next == this.head) {
                break;
            }
            curr = curr.next;
        }
        return l;
    }
}
