package fr.inria.contraintes.biocham.menus;


import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JMenu;

public class CustomColorMenu extends JMenu{
	
	
	
	//class GradientMenuItem extends JMenuItem {
		public CustomColorMenu(String text) {
			super(text);
			setOpaque(false);
			setBackground(Utils.backgroundColor);
		}
	 
		@Override
		protected void paintComponent(Graphics g) {
			Graphics2D g2 = (Graphics2D) g.create();
			int w = getWidth();
			int h = getHeight();			
			g2.setPaint(Utils.backgroundColor);
			g2.fillRect(0, 0, w, h);
			g2.dispose();
			super.paintComponent(g);
		}
	//}

}
