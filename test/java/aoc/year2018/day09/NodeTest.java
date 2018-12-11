package aoc.year2018.day09;

import org.junit.jupiter.api.Test;

import static org.hamcrest.Matchers.contains;
import static org.junit.jupiter.api.Assertions.*;
import static org.hamcrest.MatcherAssert.assertThat;


class NodeTest {
    @Test
    void newNode() {
        Node node = new Node(0);
        assertEquals(0, node.next.value);
        assertEquals(0, node.prev.value);
        assertThat(node.toList(), contains(0));
    }

    @Test
    void insertAfter1() {
        Node node1 = new Node(0);
        Node node2 = node1.insertAfter(1);

        assertEquals(0, node1.value);
        assertEquals(1, node2.value);
        assertEquals(1, node1.next.value);
        assertEquals(1, node1.prev.value);
        assertEquals(0, node2.next.value);
        assertEquals(0, node2.prev.value);
        assertThat(node1.toList(), contains(0, 1));
    }

    @Test
    void insertAfter2() {
        Node node1 = new Node(0);
        node1.insertAfter(1);
        Node node2 = node1.insertAfter(2);

        assertEquals(0, node1.value);
        assertEquals(2, node1.next.value);
        assertEquals(1, node1.prev.value);
        assertEquals(2, node2.value);
        assertEquals(1, node2.next.value);
        assertEquals(0, node2.prev.value);
        assertThat(node1.toList(), contains(0, 2, 1));
    }

    @Test
    void removeBefore() {
        Node node = new Node(0);
        node.insertAfter(1);
        node.insertAfter(2);
        node.insertAfter(3);
        int value = node.removeBefore();

        assertEquals(1, value);
        assertEquals(0, node.value);
        assertEquals(3, node.next.value);
        assertEquals(2, node.prev.value);
        assertThat(node.toList(), contains(0, 3, 2));
    }
}