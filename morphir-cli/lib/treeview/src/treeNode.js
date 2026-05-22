"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.TreeNode = void 0;
class TreeNode {
    constructor(name, type) {
        this.name = name;
        this.type = type;
        this.children = [];
        this.definition = [];
    }
}
exports.TreeNode = TreeNode;
