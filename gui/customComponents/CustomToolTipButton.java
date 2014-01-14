package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Icons;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.ModernBalloonStyle;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;



/**
 * Class thats creates custom JButton with a custom tooltip attached(BalloonTip) for the purposes of a command(button) function's explanation.
 * 
 * @author Dragana Jovanovska  
 */ 
public class CustomToolTipButton extends JButton implements MouseListener{

	BalloonTip tp;
	String texto="";
	public static final Color color1=new Color(153,204,255);//blue, 
	public static final Color color2=new Color(230,230,230);//grey
	public static final Color color3=new Color(204,204,204);//darker grey
	
	public CustomToolTipButton(){
		super();
		
		ModernBalloonStyle modern=new ModernBalloonStyle(10,10,color1,color2,color3);
		modern.setBorderThickness(3);
		modern.enableAntiAliasing(true);
		
		tp=new BalloonTip(this,"",modern,BalloonTip.Orientation.RIGHT_ABOVE,BalloonTip.AttachLocation.NORTHEAST,20,10,false);
		tp.enableClickToClose(true);
	}
	
	public CustomToolTipButton(ImageIcon l,String ttt){
		super(l);		
		ModernBalloonStyle modern=new ModernBalloonStyle(10,10,color1,color2,color3);
		modern.setBorderThickness(3);
		modern.enableAntiAliasing(true);
		tp=new BalloonTip(this,ttt,modern,BalloonTip.Orientation.RIGHT_ABOVE,BalloonTip.AttachLocation.NORTHEAST,20,10,false);
		tp.setText(ttt);
		tp.setIcon(Icons.icons.get("flag_blue.png"));
		tp.setIconTextGap(10);
		tp.enableClickToClose(true);
		tp.enableClickToHide(true);
	}
	
	public CustomToolTipButton(String s,String ttt){
		super(s);
				
		addMouseListener(this);
		ModernBalloonStyle modern=new ModernBalloonStyle(10,10,color1,color2,color3);
		modern.setBorderThickness(3);
		modern.enableAntiAliasing(true);
		tp=new BalloonTip(this,ttt,modern,BalloonTip.Orientation.RIGHT_ABOVE,BalloonTip.AttachLocation.NORTHEAST,20,10,false);
		tp.setText(ttt);
		tp.setIcon(Icons.icons.get("flag_blue.png"));
		tp.setIconTextGap(10);
		tp.enableClickToClose(true);
		tp.enableClickToHide(true);
	}
	public void setToolTipTexto(String s){
		texto=s;
	}
	public void setBalloonToolTipVisible(boolean b){
		if(b){
			tp.setVisible(true);
			
		}else{
			tp.setVisible(false);
		}
	}

	public void mouseClicked(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseEntered(MouseEvent e) {
		if(e.getSource() instanceof CustomToolTipButton){
			CustomToolTipButton b=(CustomToolTipButton)e.getSource();
			b.setBalloonToolTipVisible(true);			
		}
	}
	public void mouseExited(MouseEvent e) {		
		if(e.getSource() instanceof CustomToolTipButton){
			CustomToolTipButton b=(CustomToolTipButton)e.getSource();
			b.setBalloonToolTipVisible(false);
		}
	}

	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	
}
