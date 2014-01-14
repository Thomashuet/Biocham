package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;

import javax.swing.JMenu;
import javax.swing.JPopupMenu;

public class CustomPopupCmdLine  extends CustomPopupMenu{

	
	JPopupMenu menu;
	BiochamDynamicTree tree;
	ElementMenuCmdLine main;
	
	public CustomPopupCmdLine(BiochamDynamicTree tree){
		this.tree=tree;
	}
	
	
	public JPopupMenu getPopup(){		
	//	menu=(tree.getCmdLineMenu().refreshedPopup(tree.currentModel));
		return menu;	
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
		main.refreshed(m);
		return this.getMenu();
	}


	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		main.refreshed(m);
		return this.getPopup();
	}
}
