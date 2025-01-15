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
import org.bukkit.event.player.PlayerQuitEvent;

public class DutyListener implements Listener {
    private static MyScheduledTask dutyTask;

    @EventHandler
    public void onDutyJoin(final DutyJoinEvent event) {
        String staff = event.getStaff();

        dutyTask = DutyCore.getInstance().getScheduler().runTaskTimer(() -> {
            DutyCore.getDatabase().updateDutyTime(staff);
            DutyCore.getDatabase().updateServedTime(staff);
        }, 20L, 20L);
    }

    @EventHandler
    public void onDutyLeave(final DutyLeaveEvent event) {
        dutyTask.cancel();
        DutyCore.getDatabase().clearServedTime(event.getStaff());
    }

    @EventHandler
    public void onMove(final PlayerMoveEvent event) {
        Player player = event.getPlayer();

        if (CommandDuty.getFreezeList().contains(player)) event.setCancelled(true);
    }

    @EventHandler
    public void onQuit(final PlayerQuitEvent event) {
        Player player = event.getPlayer();

        if (DutyCore.getDatabase().isInDuty(player.getName())) DutyCore.getDatabase().leaveDuty(player.getName());
    }
}
