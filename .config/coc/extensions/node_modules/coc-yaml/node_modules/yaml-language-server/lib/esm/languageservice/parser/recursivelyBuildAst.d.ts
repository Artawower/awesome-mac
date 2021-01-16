import * as Yaml from 'yaml-ast-parser-custom-tags';
import { ASTNode } from '../jsonASTTypes';
export default function recursivelyBuildAst(parent: ASTNode, node: Yaml.YAMLNode): ASTNode;
