package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.utils.Utils;

import javax.swing.JPopupMenu;


public abstract class CustomPopupMenu extends ElementMenu{
	
		
	public abstract JPopupMenu getPopup();
		
	public JPopupMenu newPopupMenu() {
		
		JPopupMenu menu = new JPopupMenu(); 
		menu.setFont(Utils.treeExplorerFont);
		menu.setForeground(Utils.foregroundColor);
		menu.setBackground(Utils.backgroundColor);
		
		return menu;
	}
	
	
	
	

}
