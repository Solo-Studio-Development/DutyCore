package net.solostudio.dutycore.listeners;

import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.menu.Menu;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.player.PlayerJoinEvent;

public class MenuListener implements Listener {
    @EventHandler
    public void onClick(final InventoryClickEvent event) {
        if (event.getInventory().getHolder() instanceof Menu menu) {
            event.setCancelled(true);
            menu.handleMenu(event);
        }
    }
}
