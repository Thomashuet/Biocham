package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class ElementMenuWarnings extends ElementMenu{

	ColorMenu menu;
	JPopupMenu pMenu;
	BiochamDynamicTree tree;
	
	public ElementMenuWarnings(BiochamDynamicTree t){		
		tree=t;
		createMenu();
		createPopupMenu();		
	}
	
	
	private void createMenu(){
			
		
		menu=BiochamMenuBar.createNewMenu("Warnings", 0);		
		JMenuItem save = newMenuItem("Save to file"); 
		save.setActionCommand("saveWarningsFile");
		save.addActionListener(tree.treeListener);   
	    
	    JMenuItem clear = newMenuItem("Clear"); 
	    clear.setActionCommand("clearWarningsFile");
	    clear.addActionListener(tree.treeListener);	 
	    menu.add(save);		    
    	menu.add(clear);
		this.setMenu(menu);
	}
	
	
	private void createPopupMenu(){
	
		
		pMenu=BiochamMenuBar.createNewPopupMenu();
		JMenuItem save = newMenuItem("Save to file"); 
		save.setActionCommand("saveWarningsFile");
		save.addActionListener(tree.treeListener);   
	    
	    JMenuItem clear = newMenuItem("Clear"); 
	    clear.setActionCommand("clearWarningsFile");
	    clear.addActionListener(tree.treeListener);	 
	    pMenu.add(save);		    
    	pMenu.add(clear);
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
		return this.menu;
	}


	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}
}

