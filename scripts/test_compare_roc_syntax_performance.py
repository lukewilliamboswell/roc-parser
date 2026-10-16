import unittest
import argparse
import compare_roc_syntax_performance as comparison

class Worker:
    def __init__(self, per_iteration=10000, accepted=True):
        self.requests = []
        self.per_iteration = per_iteration
        self.accepted = accepted
    def request(self, request):
        self.requests.append(request)
        n = request['benchmark_iterations']
        return {'iterations': n, 'successes': n if self.accepted else 0,
                'consumed': n, 'elapsed_ns': n * self.per_iteration}

class ComparisonTests(unittest.TestCase):
    def test_calibration_warmup_and_samples(self):
        worker = Worker()
        result = comparison.measure(worker, {'id':'x','mode':'file','source':'x=1'}, True, 1000000, 5)
        self.assertEqual(result['median_ns'], 10000)
        self.assertEqual(len(result['samples_ns']), 5)
        self.assertGreaterEqual(result['iterations'] * 10000, 1000000)
        self.assertEqual(worker.requests[0]['benchmark_warmup'], 2)
    def test_noisy_reference_sample_does_not_fail_ratchet(self):
        result = comparison.ratios({'median_ns':100000,'samples_ns':[99000,100000,101000]}, {'median_ns':1000,'samples_ns':[1000,1000,10000]}, 20, 1000)
        self.assertGreater(result['reference_ratio'],20)
        self.assertFalse(result['regression'])
    def test_tenfold_anomaly_and_timing_floor(self):
        result = comparison.ratios({'median_ns':100000000,'samples_ns':[100000000]*5}, {'median_ns':10000000,'samples_ns':[10000000]*5}, 5, 1000)
        self.assertTrue(result['regression'])
        self.assertEqual(result['reference_ratio'],10)
        self.assertTrue(comparison.ratios({'median_ns':42000,'samples_ns':[42000]*5}, {'median_ns':1000,'samples_ns':[1000]*5},20,1000)['regression'])
        self.assertEqual(comparison.ratios({'median_ns':100,'samples_ns':[100]}, {'median_ns':0,'samples_ns':[0]},20,1000)['reference_ratio'],0.1)
    def test_failed_correctness_and_zero_timing_cannot_pass(self):
        with self.assertRaises(ValueError): comparison.measure(Worker(accepted=False), {'id':'x'}, True,1000,1)
        with self.assertRaises(ValueError): comparison.measure(Worker(0), {'id':'x'}, True,1000,1,max_iterations=2)
    def test_only_documented_projection_depth_limits_may_be_cut_off(self):
        self.assertEqual(comparison.shape_agrees(['binary','+',1,2], ['unsupported','depth_limit']),1)
        with self.assertRaises(ValueError): comparison.shape_agrees(['unsupported','NewNode'],['unsupported','NewNode'])
        with self.assertRaises(ValueError): comparison.shape_agrees(['number','1'],['number','2'])
    def test_nonfinite_thresholds_cannot_disable_ratchet(self):
        for value in ['nan', 'NaN', 'inf', '-inf', '0', '-1']:
            with self.assertRaises(argparse.ArgumentTypeError): comparison.positive_finite(value)
        for value in ['0', '102']:
            with self.assertRaises(argparse.ArgumentTypeError): comparison.bounded_samples(value)
    def test_smoke_is_small_and_fixture_independent(self):
        self.assertEqual(len(comparison.inputs([1,2],[4096],['missing.json'],True)),2)

if __name__ == '__main__': unittest.main()
