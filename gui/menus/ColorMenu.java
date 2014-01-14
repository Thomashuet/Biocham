package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;

import javax.swing.JMenu;

public class ColorMenu extends JMenu{
	
	public ColorMenu(){
		super();
		
		super.setContentAreaFilled(true);
		setBackground(Utils.backgroundColor);	
		setFont(Utils.treeExplorerFont);
		setForeground(Utils.foregroundColor);
		setOpaque(true);
	}
	
	public ColorMenu(String t){
		super(t);
		
		super.setContentAreaFilled(true);
		setBackground(Utils.backgroundColor);	
		setFont(Utils.treeExplorerFont);
		setForeground(Utils.foregroundColor);
		setOpaque(true);
	}

}
