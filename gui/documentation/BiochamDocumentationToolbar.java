package fr.inria.contraintes.biocham.documentation;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.AbstractAction;
import javax.swing.JLabel;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;

public class BiochamDocumentationToolbar{

	JToolBar tb;
	
	public BiochamDocumentationToolbar(){
		
		tb=new JToolBar();
		tb.setRollover(true);		
		addSeparator();
		tb.setBackground(Utils.backgroundColor);	       
		tb.setFloatable(false);
		tb.setMargin(new Insets(0,0,0,0));
		tb.setRollover(true);	    
					
		
		JLabel workbench=new JLabel("Back",Icons.icons.get("undo1.png"+0.4),SwingConstants.TRAILING);		
		workbench.setDisplayedMnemonic(KeyEvent.VK_B);
		workbench.setForeground(Color.BLUE);
		workbench.setFont(new Font("",Font.BOLD,14));
		workbench.setToolTipText("Go to Workbench...");	   
		workbench.setName("workbench");
		workbench.addMouseListener(new MouseAdapter(){

			public void mouseClicked(MouseEvent e) {
				BiochamMainFrame.showWorkbenchScreen();				
			}
		});		
		tb.add(workbench);
		addSeparator();
		addSeparator();
		
	}
	
	private void addSeparator(){
		
		JLabel sep=new JLabel("  ");		
		sep.setBackground(Utils.backgroundColor);
		sep.setForeground(Utils.backgroundColor);
		tb.add(sep);
	}

	public JToolBar getTb() {
			
		return tb;
	}
}
