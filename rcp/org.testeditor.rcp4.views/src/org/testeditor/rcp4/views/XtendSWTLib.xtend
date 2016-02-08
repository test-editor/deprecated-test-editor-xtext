package org.testeditor.rcp4.views

import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Composite
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.MessageBox
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.widgets.Text
import org.eclipse.swt.widgets.Group
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Tree
import org.eclipse.jface.viewers.TreeViewer

public class XtendSWTLib {


	def static Shell newShell(Display disp, Procedure1<Shell> init) {
		val result = new Shell(disp);
		init.apply(result);
		return result;
	}
	
	def static newButton(Composite parent, int style,
			Procedure1<Button> btn) {
		val b = new Button(parent, style);
		btn.apply(b);
		return b;
	}

	def static newButton(int style, Composite parent,
			Procedure1<Button> btn) {
		val b = new Button(parent, style);
		btn.apply(b);
		return b;
	}

	def static GridData newGridData(Procedure1<GridData> init) {
		val gd = new GridData();
		init.apply(gd);
		return gd;
	}
	
	def static Tree newTree(Composite parent, int style, Procedure1<Tree> init){
		val tree=new Tree(parent, style)
		init.apply(tree)
		return tree
	}

	def static TreeViewer newTreeViewer(Composite parent, int style, Procedure1<TreeViewer> init){
		val treeViewer=new TreeViewer(parent, style)
		init.apply(treeViewer)
		return treeViewer
	}

	def static newLabel(Composite parent, int style,
			Procedure1<Label> init) {
		val label = new Label(parent, style);
		init.apply(label);
		return label;
	}

	def static newText(Composite parent, int style,
			Procedure1<Text> init) {
		val text = new Text(parent, style);
		init.apply(text);
		return text;
	}

	def static  newMessageBox(Shell shell, int style,
			Procedure1<MessageBox> init) {
		val box = new MessageBox(shell);
		init.apply(box);
		return box;
	}

	def static  newGroup(Composite parent, int style,
			Procedure1<Group> init) {
		val group = new Group(parent, style);
		init.apply(group);
		return group;
	}

}
