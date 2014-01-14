package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class ElementMenuCmdLine extends ElementMenu{

	ColorMenu menu;
	JPopupMenu pMenu;
	BiochamDynamicTree tree;
	JMenuItem addMenuItem,paddMenuItem,saveMenuItem,psaveMenuItem;
	
	public ElementMenuCmdLine(BiochamDynamicTree t){		
		tree=t;
		createMenu();
		createPopupMenu();		
	}
	
	
	private void createMenu(){
			
		
		menu=BiochamMenuBar.createNewMenu("CommandLine", 0);		
		addMenuItem = newMenuItem("Clear"); 
		addMenuItem.setActionCommand("clearCmdLine");
		saveMenuItem = newMenuItem("Save"); 
		saveMenuItem.setActionCommand("saveCmdLine");
		menu.add(addMenuItem);
	    menu.add(saveMenuItem);	  
		this.setMenu(menu);
	}
	
	
	private void createPopupMenu(){
	
		
		pMenu=BiochamMenuBar.createNewPopupMenu();
		paddMenuItem = newMenuItem("Clear"); 
		paddMenuItem.setActionCommand("clearCmdLine");
		psaveMenuItem = newMenuItem("Save"); 
		psaveMenuItem.setActionCommand("saveCmdLine");		
		pMenu.add(paddMenuItem);
		pMenu.add(psaveMenuItem);	 
		this.setPMenu(pMenu);
	}
	
	
	@Override
	public void setMenu(ColorMenu m) {
		menu=m;		
	}
	@Override
	public void setPMenu(JPopupMenu m) {
		pMenu=m;		
	}
	
	public ColorMenu getMenu(){
		return menu;
	}
	public JPopupMenu getPMenu(){
		return pMenu;
	}


	@Override
	public ColorMenu refreshed(BiochamModel m) {
		

		if(addMenuItem.getActionListeners().length>0){
			 for(int i=0;i<addMenuItem.getActionListeners().length;i++){
				 addMenuItem.removeActionListener(addMenuItem.getActionListeners()[i]);
			 }
		 }
		addMenuItem.addActionListener(tree.treeListener);
		if(paddMenuItem.getActionListeners().length>0){
			 for(int i=0;i<paddMenuItem.getActionListeners().length;i++){
				 paddMenuItem.removeActionListener(paddMenuItem.getActionListeners()[i]);
			 }
		 }
		paddMenuItem.addActionListener(tree.treeListener);
		if(saveMenuItem.getActionListeners().length>0){
			 for(int i=0;i<saveMenuItem.getActionListeners().length;i++){
				 saveMenuItem.removeActionListener(saveMenuItem.getActionListeners()[i]);
			 }
		 }
		saveMenuItem.addActionListener(tree.treeListener);
		if(psaveMenuItem.getActionListeners().length>0){
			 for(int i=0;i<psaveMenuItem.getActionListeners().length;i++){
				 psaveMenuItem.removeActionListener(psaveMenuItem.getActionListeners()[i]);
			 }
		 }
		psaveMenuItem.addActionListener(tree.treeListener);
		return this.menu;
	}


	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}
}

