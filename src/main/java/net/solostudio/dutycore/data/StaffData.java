package net.solostudio.dutycore.data;

import org.jetbrains.annotations.NotNull;

public record StaffData(@NotNull String name,
                        boolean isInDuty,
                        @NotNull String playerRank,
                        @NotNull String staffRank,
                        @NotNull String dutyTime,
                        int bans,
                        int kicks,
                        int mutes,
                        @NotNull String badge) {
}
