package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;



public class CustomPopupMain extends CustomPopupMenu{

	JPopupMenu menu;
	BiochamDynamicTree tree;
	
	
	public CustomPopupMain(BiochamDynamicTree tree){
		this.tree=tree;
		create();
	}
	
	
	public JPopupMenu getPopup(){	
		
		return menu;
	}
		

	private void create() {
		
		menu = newPopupMenu(); 
				
		JMenuItem openMenuItem = newMenuItem("Open BIOCHAM Model"); 
		openMenuItem.setActionCommand("openBCmodel");
	    openMenuItem.addActionListener(this.tree.treeListener);
	    menu.add(openMenuItem);
	   
	    JMenuItem createMenuItem = newMenuItem("New BIOCHAM Model");    
	    createMenuItem.setActionCommand("newBCmodel");
	    createMenuItem.addActionListener(this.tree.treeListener);
	    menu.add(createMenuItem);
	    
	    menu.addSeparator();
	    
	    JMenuItem clearMenuItem = newMenuItem("Close All Opened Models");    
    	clearMenuItem.setActionCommand("clearAll");
    	clearMenuItem.addActionListener(this.tree.treeListener);
    	menu.add(clearMenuItem);
    
	}	
	@Override
	public ColorMenu getMenu() {
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public JPopupMenu getPMenu() {
		// TODO Auto-generated method stub
		return menu;
	}


	@Override
	public void setMenu(ColorMenu menu) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void setPMenu(JPopupMenu menu) {
		this.menu=menu;
		
	}



	@Override
	public ColorMenu refreshed(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}
}
	