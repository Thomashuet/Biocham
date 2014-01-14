package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.utils.Utils;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public abstract class ElementMenu{

		
	public ColorMenu newMenu(String name) {
		
		ColorMenu menu = new ColorMenu(name);
		
		return menu;
	}
	
	public JMenuItem newMenuItem(String name,ImageIcon icon) {
	//	ImageIcon bwIcon =Icons.icons.get("article-32.png");
		JMenuItem menu = new JMenuItem(name,icon); 
		menu.setBackground(Utils.backgroundColor);
		menu.setFont(Utils.treeExplorerFont);
		menu.setForeground(Utils.foregroundColor);
		
		return menu;
	}
	
	public JMenuItem newMenuItem(String name) {
	
		JMenuItem menu = new JMenuItem(name); 
		menu.setBackground(Utils.backgroundColor);
		menu.setFont(Utils.treeExplorerFont);
		menu.setForeground(Utils.foregroundColor);
		menu.updateUI();
		return menu;
	}
	
	public static void setDummy(JPopupMenu menu,int i, boolean has) {
			menu.getComponent(i).setEnabled(has);
		
		
	}
	
	
	
	public abstract ColorMenu getMenu();
	public abstract JPopupMenu getPMenu();
	public abstract void setMenu(ColorMenu menu);
	public abstract void setPMenu(JPopupMenu menu);
	public abstract ColorMenu refreshed(BiochamModel m);
	public abstract JPopupMenu refreshedPopup(BiochamModel m);
}
