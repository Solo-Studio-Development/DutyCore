package net.solostudio.dutycore.listeners;

import com.github.Anon8281.universalScheduler.scheduling.tasks.MyScheduledTask;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.commands.CommandDuty;
import net.solostudio.dutycore.events.DutyJoinEvent;
import net.solostudio.dutycore.events.DutyLeaveEvent;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerMoveEvent;

public class DutyListener implements Listener {
    private static MyScheduledTask dutyTask;

    @EventHandler
    public void onDutyJoin(final DutyJoinEvent event) {
        dutyTask = DutyCore.getInstance().getScheduler().runTaskTimer(() -> DutyCore.getDatabase().updateDutyTime(event.getStaff()), 20L, 20L);
    }

    @EventHandler
    public void onDutyLeave(final DutyLeaveEvent event) {
        dutyTask.cancel();
    }

    @EventHandler
    public void onMove(final PlayerMoveEvent event) {
        Player player = event.getPlayer();

        if (CommandDuty.getFreezeList().contains(player)) event.setCancelled(true);
    }
}
