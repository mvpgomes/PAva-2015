public class Score {
    private int passed;
    private int failed;

    public Score() {
        this.passed = 0;
        this.failed = 0;
    }

    public int getPassed() {
        return passed;
    }

    public int getFailed() {
        return failed;
    }

    public void passedTest() {
        this.passed++;
    }

    public void failedTest() {
        this.failed++;
    }
}
